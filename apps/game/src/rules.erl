%%%-------------------------------------------------------------------
%%% @copyright
%%% Copyright (C) 2011 by Bermuda Triangle
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%% @end
%%%-------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% @doc
%% rules implements abstract game rules. The module relies
%% on a separate module (name is supplied to process) to implement
%% the specific rules, furtheron called the 'rule implementation'.
%%
%% a `#rule' is a strategy-like record,
%% that has a name, an arity, a detector function and an actor function.
%% The detector function receives a Map and an arity-tuple of orders and
%% has to return true if the actor function needs to run. By convention,
%% a detector function should run side effect free.
%%
%% The detector function receives a Map and the tuple that made the detector
%% function return true. It's responsibility is to enforce the rule it
%% implements.
%% For this, it transforms the _whole_ orders list by returning a
%% [tuple()]
%% legal tuples are:
%%  <pre>{add, Order}</pre> - adds an order to the orders list
%%  <pre>{remove, Order}</pre> - removes an order from the orders list
%%  <pre>{reply, ErlangTerm}</pre> - `ErlangTerm' will end up in the return value of
%%                          {@link rules:process/4} - this can be used to
%%                          request user actions like retreat orders.
%%
%% the `arity' field has a special case, if it is set to `all_orders', then
%% the detector/actor will be called with the whole order-list as argument.
%% This can be used to handle units that received no orders, for instance.
%%
%% The expected signature of the detector/actor funs depends on the arity as
%% well:
%% <pre>
%% arity = 0 --> spec: actor_and_detector(Map, {})
%% arity = 1 --> spec: actor_and_detector(Map, {Order})
%% arity = 2 --> spec: actor_and_detector(Map, {Order})
%% arity = all_units --> spec: actor_and_detector(Map, [Order])</pre>
%%
%% the rule engine will receive the `[#rule]' from the rule implementation
%% by calling it's create/2 function with the game type and game phase as
%% arguments.
%%
%% The rule engine will apply the rules from the rule implementation until there
%% are no changes to the order list any more.
%% After that, the remaining order list will be executed by calling
%% the rule implementation's `do_process/2' function.
%% This function receives the Map and an order and executes this order through
%% applying sideeffectful operations to the map.
%%
%% @author <stephan.brandauer@gmail.com>
%% @end
%% -----------------------------------------------------------------------------
-module (rules).

-export ([process/4]).

-include_lib ("utils/include/debug.hrl").

-include_lib ("eunit/include/eunit.hrl").
-include_lib ("game/include/rule.hrl").

-type order () :: tuple ().

-type map () :: any ().

%% -----------------------------------------------------------------------------
%% @doc
%% processes a list of orders according to the rules and returns the messages
%% the `RULES_MOD' left.
%% @end
%% -----------------------------------------------------------------------------
-spec process (any (), map (), atom (), [order ()]) -> [tuple ()].
process (Phase, Map, RULES_MOD, Orders) ->
    ?DEBUG(user, "#### process ~p~n", [Phase]),
    Rules = RULES_MOD:create (standard_game, Phase),
    {Replies, Orders1} = execute_rules (Map, Rules, Orders),
    ProcessAnswer =
        lists:foldl (fun (Order, Acc) ->
                             RULES_MOD:do_process (Phase, Map, Order) ++ Acc
                     end,
                     [],
                     Orders1),
    {ProcessReplies, _} = separate_orders_and_replies (ProcessAnswer),
    lists:foldl (fun (Item, Acc) ->
                         case Item of
                             ok -> Acc;
                             Other -> [Other | Acc]
                         end
                 end,
                 [],
                 ProcessReplies ++ Replies).

-spec make_pairs ([any ()], Arity) -> [tuple ()] when
      Arity :: pos_integer () | all_orders.
make_pairs (_, 0) ->
    [{}];
make_pairs (List, 1) ->
    [{A} || A <- List];
make_pairs (List, 2) ->
    [{A, B} || A <- List, B <- List, A < B];
make_pairs (List, all_orders) ->
    [List].

transform_orders (Orders, RuleResponse) ->
        lists:foldl (
          fun (Order, Acc) ->
                  case Order of
                      {remove, O} ->
                          lists:delete (O, Acc);
                      {add, O} ->
                          [O | Acc];
                      {reply, Term} ->
                          [{reply, Term} | Acc];
                      ok ->
                          Acc;
                      Other -> erlang:error ({error, unhandled_clause,
                                              Other,
                                              ?MODULE, ?LINE})
                  end
          end,
          Orders,
          RuleResponse).

separate_orders_and_replies (TransformedResponse) ->
    lists:foldl (
      fun (Item, {RepliesAcc, OrdersAcc}) ->
              case Item of
                  {reply, Left} ->
                      {[Left | RepliesAcc], OrdersAcc};
                  ok ->
                      {RepliesAcc, OrdersAcc};
                  Right ->
                      {RepliesAcc, [Right | OrdersAcc]}
              end
      end,
      {[],[]},
      TransformedResponse).

%% @doc
%% The function execute_single_rule is quite central to the module.
%% A #rule is a record that has a certain 'arity'.
%% A #rule of arity 2 applies to 2-tuples of orders:
%% Every possible pair of two orders is generated and given to the
%% #rule.detector function. If that returns true, an instance of
%% the rule was found. In that case, the tuple is given to the
%% #rule.actor function - that function replies with two updated orders or
%% an atom telling the execute_order function what to do with those rules
%% (for instance: 'remove', 'add')
%% @end
-spec execute_single_rule (Map, Rule, [Order]) -> {[Reply], [Order]} when
      Map :: map (),
      Rule :: #rule{},
      Reply :: any (),
      Order :: order ().
execute_single_rule (Map, Rule, Orders) ->
    Pairs = make_pairs (Orders, Rule#rule.arity),
    ?DEBUG(user, "execute rule ~p~n", [Rule#rule.name]),
    RuleResponse =
        lists:foldl (
          fun (Pair, Acc) ->
                  case (Rule#rule.detector) (Map, Pair) of
                      true ->
                          Emit = (Rule#rule.actor) (Map, Pair),
                          ?DEBUG(user,
                            "Rule ~p: Emitting '~p'~n",
                            [Rule#rule.name,
                             Emit]),
                          Emit ++ Acc;
                      _Other ->
                              Acc
                  end
          end,
          [],
          Pairs),
%    ?DEBUG(user, "executing ~p done~n", [Rule#rule.name]),
    %% execute the instructions from the actors:
    TransformedResponse =
        transform_orders (Orders, RuleResponse),
    %%make an {[Replies],[Orders]} tuple:
    separate_orders_and_replies (TransformedResponse).

%% loops over the execute_rules_stage method until a fixpoint is reached
-spec execute_rules (Map, [Rule], [Order]) -> any () when
      Map :: map (),
      Rule :: #rule{},
      Order :: order ().
execute_rules (Map, Rules, OrdersAndReplies) ->
    case execute_rules_stage (Map, Rules, OrdersAndReplies) of
        OrdersAndReplies ->
            OrdersAndReplies;
        ChangedOrders ->
            execute_rules (Map, Rules, ChangedOrders)
    end.

%% executes all rules. This could lead to incomplete evaluation of all rules,
%% because rules can add and remove Orders. This is why this function is looped
%% by execute_rules
-spec execute_rules_stage (Map, [Rule], {[Reply], [Order]} | [Order]) ->
                                  {[Reply], [Order]} when
      Map :: map (),
      Rule :: #rule{},
      Reply :: any (),
      Order :: order ().
execute_rules_stage (Map, [Rule | Rules], {Replies, Orders}) ->
    {NewReplies, NewOrders} =
        execute_rules_stage(Map,
                            Rules,
                            execute_single_rule (Map, Rule, Orders)),
    {ordsets:from_list (NewReplies ++ Replies), ordsets:from_list (NewOrders)};
execute_rules_stage (_, [], RepliesAndOrders = {_, _}) ->
    RepliesAndOrders;
execute_rules_stage (_, [], Orders) ->
    {[], Orders};
execute_rules_stage (Map, Rules, Orders) ->
    execute_rules_stage (Map, Rules, {[], Orders}).
