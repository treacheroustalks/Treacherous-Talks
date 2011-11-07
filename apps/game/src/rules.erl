%% -----------------------------------------------------------------------------
%% @doc
%% rules implements abstract game rules. The module relies
%% on a seperate module (name is supplied to process) to implement
%% the specific rules.
%%
%% @author <stephan.brandauer@gmail.com>
%% @end
%% -----------------------------------------------------------------------------
-module (rules).

-export ([process/4]).

-include_lib ("eunit/include/eunit.hrl").
-include_lib ("game/include/rule.hrl").

-type order () :: tuple ().

-type map () :: any ().

%% -----------------------------------------------------------------------------
%% @doc
%% processes a list of orders according to the rules and returns the messages
%% the RULES_MOD left.
%% @end
%% -----------------------------------------------------------------------------
-spec process (any (), map (), atom (), [order ()]) -> [tuple ()].
process (Phase, Map, RULES_MOD, Orders) ->
    io:format (user, "#### process ~p~n", [Phase]),
    Rules = RULES_MOD:create (standard_game, Phase),
    {Replies, Orders1} = execute_rules (Map, Rules, Orders),
    lists:map (fun (Order) ->
                       apply (RULES_MOD,
                              do_process,
                              [Phase, Map, Order])
               end,
               Orders1),
    Replies.

-spec make_pairs ([any ()], Arity) -> [tuple ()] when
      Arity :: pos_integer ().
make_pairs (_, 0) ->
    [{}];
make_pairs (List, 1) ->
    [{A} || A <- List];
make_pairs (List, 2) ->
    [{A, B} || A <- List, B <- List, A < B].

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
    RuleResponse =
        lists:foldl (
          fun (Pair, Acc) ->
                  case (Rule#rule.detector) (Map, Pair) of
                      true ->
                          Emit = (Rule#rule.actor) (Map, Pair),
                          io:format (user,
                                     "Rule ~p: Emitting '~p'~n",
                                     [Rule#rule.name,
                                      Emit]),
                          Emit ++ Acc;
                      false ->
                              Acc
                  end
          end,
          [],
          Pairs),
    %% execute the instructions from the actors:
    TransformedResponse =
        lists:foldl (
          fun (Order, Acc) ->
                  case Order of
                      {remove, O} ->
                          lists:delete (O, Acc);
                      {add, O} ->
                          [O | Acc];
                      {reply, Term} ->
                          [{reply, Term} | Acc];
                      Other -> erlang:error ({error, unhandled_clause,
                                              Other,
                                              ?MODULE, ?LINE})
                  end
          end,
          Orders,
          RuleResponse),
    %% distinguish orders and replies:
    lists:foldl (
      fun (Item, {RepliesAcc, OrdersAcc}) ->
              case Item of
                  {reply, Reply} ->
                      {[Reply | RepliesAcc], OrdersAcc};
                  DistOrder ->
                      {RepliesAcc, [DistOrder | OrdersAcc]}
              end
      end,
      {[],[]},
      TransformedResponse).

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

%% executes all rules. This could lead to inconsistencies, because
%% rules can add and remove Orders. This is why this function is looped
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
