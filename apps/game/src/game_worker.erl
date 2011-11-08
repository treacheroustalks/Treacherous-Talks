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
-export([get_game_order/2,translate_game_order/3]).

%% server state
-record(state, {}).



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


handle_call({put_game_order, Key, GameOrderList}, _From, State) ->
    Reply = put_game_order(Key, GameOrderList),
    {reply, Reply, State};
handle_call({put_game_order, GameId, UserId, GameOrderList}, _From, State) ->
    Reply = put_game_order(GameId, UserId, GameOrderList),
    {reply, Reply, State};

handle_call({update_game_order, Key, GameOrderList}, _From, State) ->
    Reply = update_game_order(Key, GameOrderList),
    {reply, Reply, State};
handle_call({get_game_order, Key}, _From, State) ->
    Reply = get_game_order(Key),
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
handle_call(_Request, _From, State) ->
    io:format ("received unhandled call: ~p~n",[{_Request, _From, State}]),
    {noreply, ok, State}.

handle_cast({phase_change, Game, NewPhase}, State) ->
    phase_change(Game, NewPhase),
    {noreply, State};
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

put_game_order(GameId, UserId, GameOrderList) ->
    case get_game(GameId) of
        {ok, _Game = #game{} } ->
            YearSeason = "1900-fall",
            Phase = "phase",
            case get_player_country(GameId, UserId) of
                {ok, Country} ->
                    Key = integer_to_list(GameId) ++ YearSeason ++
                                                  Phase ++ atom_to_list(Country),
                    put_game_order(Key,
                           translate_game_order(GameId, GameOrderList,Country));
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
    BinID = db:int_to_bin(ID),
    DBReply = db:get(?B_GAME, BinID),
    case DBReply of
        {ok, DBObj} ->
            {ok, db_obj:get_value (DBObj)};
        Other ->
            Other
    end.

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
    BinID = db:int_to_bin(GameID),
    DBReply = db:get (?B_GAME_PLAYER, BinID),
    case DBReply of
        {ok, DBObj} ->
            {ok, db_obj:get_value (DBObj)};
        Other ->
            Other
    end.

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
        _ -> %TODO provide state for other type of games which are not waiting
            {error, game_not_waiting}
    end.


phase_change(Game, NewPhase) ->
    case NewPhase of
        order_phase ->
            ok;
        retreat_phase ->
            %% after evaluating the orders, and retreat phase is not needed
            %% send back an event game_timer(Gameid, Event)
            %% to skip retreat phase: game_timer:event(Game#game.id, skip);
            ok;
        build_phase ->
            %% check if build phase is needed, if not, send an event
            %% to the game_timer
            %% to skip phase if not needed: game_timer:event(Game#game.id, skip)
            ok;
        started ->
            %% update the game in the db (it is now ongoing)
            %% this is only the first time, continue as the order_phase case
            update_game(Game#game.id, Game),
            %% do some other stuff that's needed...
            game_join_proc:stop(Game#game.id),
            ok
    end.

get_game_order(GameId, _UserId)->
    YearSeason = "1900-fall",
    Phase = "phase",
    Country = "country",
    Key = integer_to_list(GameId) ++ YearSeason ++ Phase ++ Country,
    get_game_order(Key).
get_game_order(ID)->
    BinID = list_to_binary(ID),
    DBReply = db:get(?B_GAME_ORDER, BinID),
    case DBReply of
        {ok, DBObj} ->
            {ok, db_obj:get_value (DBObj)};
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
