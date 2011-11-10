-module(game_worker).
-behaviour(gen_server).

-include_lib ("datatypes/include/game.hrl").
-include_lib ("datatypes/include/bucket.hrl").
-include_lib ("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, ping/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% export for eunit
-export([get_game_order/1, translate_game_order/3]).

%% server state
-record(state, {}).

%% define start year
-define(START_YEAR, 1900).
%% define rules module
-define(RULES, diplomacy_rules).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, no_arg, []).

ping() ->
    gen_server:call(service_worker:select_pid(?MODULE), ping).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init (no_arg) -> no_return ().
init(no_arg) ->
    service_worker:join_group(?MODULE),
    {ok, #state{}}.

handle_call(ping, _From, State) ->
    {reply, {pong, self()}, State};

handle_call({put_game_order, GameId, UserId, GameOrderList}, _From, State) ->
    Reply = put_game_order(GameId, UserId, GameOrderList),
    {reply, Reply, State};

handle_call({new_game, Game=#game{id = ID}}, _From, State) ->
    Reply = new_game(ID, Game),
    {ok, NewID} = Reply,
    {ok, NewGame} = get_game(NewID),
    game_timer_sup:create_timer(NewGame),
    game_timer:event(NewID, start),
    {reply, Reply, State};
handle_call({reconfig_game, Game=#game{id = ID}}, _From, State) ->
    Reply = update_game(ID, Game),
    game_timer:event(ID, {reconfig, Game}),
    {reply, Reply, State};
handle_call({get_game, ID}, _From, State) ->
    Reply = get_game(ID),
    {reply, Reply, State};
handle_call({get_keys_by_idx, Field, Value}, _From, State) ->
    Reply = get_keys_by_idx(Field, Value),
    {reply, Reply, State};

handle_call({join_game, GameID, UserID, Country}, _From, State) ->
    Reply = game_join_proc:join_game(GameID, UserID, Country),
    {reply, Reply, State};
handle_call({get_game_player, GameID}, _From, State) ->
    Reply = get_game_player(GameID),
    {reply, Reply, State};
handle_call({get_game_state, GameID, UserID}, _From, State) ->
    Reply =get_game_state(GameID, UserID),
    {reply, Reply, State};
handle_call({delete_game, Key}, _From, State) ->
    BinKey = list_to_binary(integer_to_list(Key)),
    Reply = db:delete(?B_GAME, BinKey),
    {reply, Reply, State};
handle_call({process_phase, ID, Phase}, _From, State) ->
    Reply = process_phase(ID, Phase),
    {reply, Reply, State};
handle_call({get_current_game, ID}, _From, State) ->
    Reply = get_current_game(ID),
    {reply, Reply, State};
handle_call({phase_change, Game, NewPhase},_From, State) ->
    Reply = phase_change(Game, NewPhase),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    io:format ("received unhandled call: ~p~n",[{_Request, _From, State}]),
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    io:format ("received unhandled cast: ~p~n",[{_Msg, State}]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format ("[~p] terminated ~p: reason: ~p, state: ~p ~n",
               [?MODULE, self(), _Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_player_country (GameID, UserID) ->
    case get_game_player(GameID) of
        {ok, GPRec = #game_player{}} ->
            case lists:keyfind(UserID, #game_user.id,
                               GPRec#game_player.players) of
                false ->
                    {error, user_not_playing_this_game};
                GU = #game_user{} ->
                    {ok, GU#game_user.country}
            end;
        Other ->
            Other

    end.

%% ------------------------------------------------------------------
%% @doc
%% GameId::integer(), UserId::integer()
%%
%% @end
%% ------------------------------------------------------------------
put_game_order(GameId, UserId, GameOrderList) ->
    case get_game(GameId) of
        {ok, _Game = #game{} } ->
            case get_player_country(GameId, UserId) of
                {ok, Country} ->
                    YearSeasonPhase = "-1900-fall-order-",
                    Key = integer_to_list(GameId) ++ YearSeasonPhase ++
                          atom_to_list(Country),
                    NewOrder = translate_game_order(GameId, GameOrderList,
                                                    Country),
                    % if player hasn't sent any order, use put, otherwise update
                    case get_game_order (Key) of
                        {ok, _} ->
                            update_game_order(Key, NewOrder);
                        {error, notfound} ->
                            put_game_order(Key, NewOrder);
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        _ ->
            {error, game_id_not_exist}
    end.

%%------------------------------------------------------------------------------
%% @doc
%%   to translate the pasred user entry for order to agree with our rule engine
%% @end
%%------------------------------------------------------------------------------
translate_game_order(GameId, GameOrderList,Country) ->
    case get_game_map(GameId, #game_overview{}) of
        {ok, _GOV = #game_overview{map = MapTerm}} ->
            Map = digraph_io:from_erlang_term(MapTerm),
            translate_game_order(GameId, GameOrderList,Country, [], Map);
        Error ->
            Error
    end.

translate_game_order(_GameId, [],_Country, Acc, _Map) ->
    Acc;
translate_game_order(GameId, [H|Rest],Country, Acc, Map) ->
    Type=element(1,H),
    TranslatedOrder =
        case Type of
            move ->
                {_, Unit, From, To, _} = H,
                {move, {Unit, Country}, From, To};
            hold ->
                {_, Unit, Wh} = H,
                {hold, {Unit, Country}, Wh};
            support_move ->
                {_, SupUnit, SupportWh, _, From, To, _} = H,
                case map:get_units(Map, From) of
                    [] ->
                        [];
                    Result when length(Result) > 1 ->
                        [];
                    [{Unit, UConutry}] ->
                        {support, {SupUnit, Country}, SupportWh,
                         {move, {Unit, UConutry}, From, To}}
                end;
            support_hold ->
                {_, SupUnit, SupportWh, _, Wh} = H,
                case map:get_units(Map, Wh) of
                    [] ->
                        [];
                    Result when length(Result) > 1 ->
                        [];
                    [{Unit, UConutry}] ->
                        {support, {SupUnit, Country}, SupportWh,
                         {hold, {Unit, UConutry}, Wh}}
                end;
            convoy ->
                {_, Fleet, Wh, _, From, To} = H,
                case map:get_units(Map, From) of
                    [] ->
                        [];
                    Result when length(Result) > 1 ->
                        [];
                    [{Army, UConutry}] ->
                        {convoy, {Fleet, Country}, Wh,
                         {Army, UConutry}, From, To}
                end;
            build ->
                {_, Unit, Wh, _} = H,
                {build, {Unit, Country}, Wh};
            remove ->
                {_, Unit, Wh} = H,
                {destroy, {Unit, Country}, Wh}
        end,
    case TranslatedOrder of
        [] ->
            translate_game_order(GameId, Rest,Country, Acc, Map);
        _->
            translate_game_order(GameId, Rest,Country, [TranslatedOrder|Acc], Map)
    end.

put_game_order(Key, GameOrderList) ->
    BinID = list_to_binary(Key),
    DBGameOrderObj = db_obj:create (?B_GAME_ORDER, BinID, GameOrderList),
    db:put (DBGameOrderObj),
    {ok, Key}.

update_game_order(ID, NewOrder) ->
    case db:get(?B_GAME_ORDER, list_to_binary(ID)) of
        {ok, Obj} ->
            NewObj = db_obj:set_value(Obj, NewOrder),
            db:put(NewObj),
            {ok, NewOrder};
        Error ->
            {error, Error}
    end.

new_game(undefined, #game{} = Game) ->
    ID = db:get_unique_id(),
    new_game(ID, Game#game{id = ID});
new_game(ID, #game{} = Game) ->
    BinID = db:int_to_bin(ID),
    DBGameObj=db_obj:create(?B_GAME, BinID, Game),
    DBGamePlayerObj=db_obj:create (?B_GAME_PLAYER, BinID, #game_player{id=ID}),
    GameObjWithIndex = db_obj:set_indices(DBGameObj, create_idx_list(Game)),
    GamePutResult = db:put (GameObjWithIndex),
    case GamePutResult of
        {error, _} = Error ->
            Error;
        _ ->
            PlayersPutResult = db:put (DBGamePlayerObj),
            case PlayersPutResult of
                {error, _} = Error ->
                    Error;
                _ ->
                    {ok, _Pid} = game_join_proc:start(ID),
                    {ok, ID}
            end
    end.

update_game(ID, #game{} = Game) ->
    ?debugMsg("Update game"),
    ?debugVal(Game),
    BinID = db:int_to_bin(ID),
    DBGameObj=db_obj:create(?B_GAME, BinID, Game),
    GameObjWithIndex = db_obj:set_indices(DBGameObj, create_idx_list(Game)),
    GamePutResult = db:put (GameObjWithIndex),
    case GamePutResult of
        {error, _} = Error ->
            Error;
        _ ->
            {ok, ID}
    end.

get_game(ID)->
    get_DB_obj(?B_GAME, db:int_to_bin(ID)).

get_keys_by_idx(Field, Val) ->
    case create_idx(Field, Val) of
        {error, field_not_indexed} ->
            {error, field_not_indexed};
        Idx ->
            case db:get_index(?B_GAME, Idx) of
                {ok, Matches} ->
                    Keys = index_result_keys(Matches),
                    {ok, Keys};
                Other ->
                    {error, Other}
            end
    end.

index_result_keys([]) ->
    [];
index_result_keys([[?B_GAME, KeyBin] | Rest]) ->
    Key = list_to_integer(binary_to_list(KeyBin)),
    [ Key | index_result_keys(Rest) ];
index_result_keys([[_, _] | Rest]) ->
    index_result_keys(Rest).

get_game_player(GameID)->
    get_DB_obj(?B_GAME_PLAYER, GameID).


get_game_state(GameID, UserID) ->
    case get_game_player(GameID) of
        {ok, GPRec = #game_player{}} ->
            case lists:keyfind(UserID, #game_user.id,
                               GPRec#game_player.players) of
                false ->
                    {error, user_not_playing_this_game};
                GU = #game_user{} ->
                    get_game_map(GameID, #game_overview
                                               {country = GU#game_user.country})
            end;
        Other ->
            Other
    end.

get_game_map(GameID, #game_overview{} = GameOverview) ->
    {ok, Game=#game{status= Status}} = get_game(GameID),
    case Status of
        waiting ->
            Map = map_data:create (standard_game),
            GameOV = GameOverview#game_overview{game_rec= Game,
                                                map = digraph_io:to_erlang_term(Map)},
            {ok, GameOV};
        ongoing -> %TODO provide state for other type of games which are not waiting
            case  get_DB_obj(?B_GAME_STATE, get_keyprefix({id, GameID})) of
                {ok, State} ->
                    Map = digraph_io:from_erlang_term(State#game_state.map),
                    GameOV = GameOverview#game_overview{game_rec = Game,
                                                        map = Map},
                    {ok, GameOV}
            end;
        _ -> {error, game_not_waiting}
    end.


phase_change(Game = #game{id = ID}, started) ->
    io:format("Game ~p started~n", [ID]),
    %% maybe tell the users about game start?
    update_game(ID, Game),
    setup_game(ID),
    game_join_proc:stop(ID);
phase_change(ID, build_phase) ->
    io:format("Game ~p entered build_phase~n", [ID]),
    Key = get_keyprefix({id, ID}),
    {ok, GameState} = get_DB_obj(?B_GAME_STATE, Key),
    % skip count phase if it is not fall
    case GameState#game_state.year_season of
        {_Year, spring} -> {ok, skip};
        _Other ->
            Map = digraph_io:from_erlang_term(GameState#game_state.map),
            _Result = rules:process(count_phase, Map, ?RULES, []),
            %% inform players of Result
            NewCurrentGame = update_current_game(ID, build_phase),
            new_state(NewCurrentGame, GameState#game_state.map),
            {ok, true}
    end;
phase_change(ID, Phase) ->
    io:format("Game ~p entered ~p~n", [ID, Phase]),
    Key = get_keyprefix({id, ID}),
    {ok, OldState} = get_DB_obj(?B_GAME_STATE, Key),
    NewCurrentGame = update_current_game(ID, Phase),
    new_state(NewCurrentGame, OldState#game_state.map).

%% ------------------------------------------------------------------
%% @doc
%% Key: "3457892458-1900-fall-order-england"
%%
%% @end
%% ------------------------------------------------------------------
get_game_order(ID)->
    get_DB_obj(?B_GAME_ORDER, ID).

setup_game(ID) ->
    %% create initial game state
    Map = digraph_io:to_erlang_term(map_data:create(standard_game)),
    GameState = #game_state{id = ID,
                            phase = order_phase,
                            year_season = spring,
                            map = Map},
    StateKey = list_to_binary(integer_to_list(ID) ++
                              "-" ++ integer_to_list(?START_YEAR) ++
                              "-" ++ "spring" ++
                              "-" ++ "order_phase"),
    DBGameState = db_obj:create(?B_GAME_STATE, StateKey, GameState),
    %% Link the game state to its game
    GameStateLinkObj = db_obj:add_link(DBGameState,
                                       {{?B_GAME,
                                         db:int_to_bin(ID)},
                                        ?GAME_STATE_LINK_GAME}),
    db:put(GameStateLinkObj),
    %% create first current game
    CurrentGame = #game_current{id = ID,
                                year_season = {?START_YEAR, spring},
                                current_phase = order_phase},
    CurrentKey = list_to_binary(integer_to_list(ID) ++ "-" ++ "current"),
    DBCurrentGame = db_obj:create(?B_GAME, CurrentKey, CurrentGame),
    %% Link the current game to its gamestate
    CurrentGameLinkObj = db_obj:add_link(DBCurrentGame,
                                         {{?B_GAME_STATE, StateKey},
                                          ?CURRENT_GAME_LINK_STATE}),
    db:put(CurrentGameLinkObj).


update_current_game(ID, Phase) ->
    BinKey = list_to_binary(integer_to_list(ID) ++ "-" ++ "current"),
    DBReply = db:get(?B_GAME, BinKey),
    case DBReply of
        {ok, CurrentGameObj} ->
            CurrGame = db_obj:get_value(CurrentGameObj),
            case Phase of
                order_phase ->
                    case CurrGame#game_current.year_season of
                        {Year, fall} ->
                            NewCurrGame =  CurrGame#game_current{
                                             year_season = {Year+1, spring},
                                             current_phase = Phase};
                        {Year, spring} ->
                            NewCurrGame = CurrGame#game_current{
                                            year_season = {Year, fall},
                                            current_phase = Phase}
                    end;
                _OtherPhase ->
                    NewCurrGame = CurrGame#game_current{
                                    current_phase = Phase}
            end,
            DBCurrGameObj=db_obj:set_value(CurrentGameObj, NewCurrGame),
            db:put (DBCurrGameObj),
            NewCurrGame;
        Other ->
            Other
    end.

%% This gets the orders for all countries in a phase
get_all_orders(ID) ->
    Keyprefix = get_keyprefix({id, ID}),
    Countries = [england, germany, france, austria, italy, russia, turkey],
    ListOrders = fun(Country, Acc) ->
                         case get_game_order(Keyprefix ++ "-" ++
                                             atom_to_list(Country)) of
                             {ok, Orders} ->
                                 lists:merge(Acc,
                                             Orders#game_order.order_list);
                             _Other ->
                                 Acc
                         end
                 end,
    lists:foldl(ListOrders, [], Countries).


get_keyprefix({id, ID}) ->
    {ok, Current} = get_current_game(ID),
    get_keyprefix({game_current, Current});
get_keyprefix({game_current, Current}) ->
    {Year, Season} = Current#game_current.year_season,
    Key = integer_to_list(Current#game_current.id) ++ "-"
        ++ integer_to_list(Year) ++ "-"
        ++ atom_to_list(Season) ++ "-"
        ++ atom_to_list(Current#game_current.current_phase),
    Key.

get_current_game(ID) ->
    get_DB_obj(?B_GAME, integer_to_list(ID) ++ "-" ++ "current").


get_DB_obj(Bucket, Key) ->
    if
        is_binary(Key) -> BinKey = Key;
        is_integer(Key) -> BinKey = db:int_to_bin(Key);
        is_list(Key) -> BinKey = list_to_binary(Key);
        is_atom(Key) -> BinKey = list_to_binary(atom_to_list(Key));
        true -> BinKey = Key % don't know what else it could be!
    end,
    DBReply = db:get(Bucket, BinKey),
    case DBReply of
        {ok, DBObj} ->
            {ok, db_obj:get_value(DBObj)};
        Other ->
            Other
    end.

%%-------------------------------------------------------------------
%% @doc
%% Creates the index list for the database
%% @end
%%-------------------------------------------------------------------
create_idx_list(#game{status=Status, press=Press, num_players=NumPlayers}) ->
    [
     create_idx(#game.status, Status),
     create_idx(#game.press, Press),
     create_idx(#game.num_players, NumPlayers)
    ].

%%-------------------------------------------------------------------
%% @doc
%% Creates an index tuple for the database.
%% @end
%%-------------------------------------------------------------------
create_idx(#game.status, Status) ->
    {<<"status_bin">>, term_to_binary(Status)};
create_idx(#game.press, Press) ->
    {<<"press_bin">>, term_to_binary(Press)};
create_idx(#game.num_players, NumPlayers) ->
    {<<"num_players_int">>, NumPlayers};
create_idx(_, _) ->
    {error, field_not_indexed}.


process_phase(ID, Phase) ->
    Key = get_keyprefix({id, ID}),
    {ok, GameState} = get_DB_obj(?B_GAME_STATE, Key),
    Map = digraph_io:from_erlang_term(GameState#game_state.map),
    ?debugVal(Orders = get_all_orders(ID)),
    io:format("Received orders: ~p~n", [Orders]),
    Result = rules:process(Phase, Map, ?RULES, Orders),
    update_state(Key, GameState#game_state{
                        map = digraph_io:to_erlang_term(Map)}),
    %% we probably want to handle the result in some way
    %% like informing the users
    {ok, Result}.


update_state(Key, NewState) ->
    {ok, OldStateObj} = db:get(?B_GAME_STATE, Key),
    DBGameObj=db_obj:set_value(OldStateObj, NewState),
    db:put(DBGameObj).

new_state(CurrentGame, Map) ->
    Key = get_keyprefix({game_current, CurrentGame}),
    GameState = #game_state{id = CurrentGame#game_current.id,
                            year_season = CurrentGame#game_current.year_season,
                            phase = CurrentGame#game_current.current_phase,
                            map = Map},
    DBGameState = db_obj:create(?B_GAME_STATE, Key, GameState),
    %% Link the game state to its game
    GameStateLinkObj = db_obj:add_link(DBGameState,
                                       {{?B_GAME,
                                         db:int_to_bin(CurrentGame#game_current.id)},
                                        ?GAME_STATE_LINK_GAME}),
    db:put(GameStateLinkObj).
