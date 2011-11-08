%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Test module for the web parser
%%% @end
%%%
%%% @since : 28 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(web_parser_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").

-export([]).

parser_test_() ->
    {inorder,
     [
      fun login/0,
      fun register/0,
      fun update/0,
      fun get_game/0,
      fun create_game/0,
      fun get_session_user/0,
      fun reconfig_game/0,
      fun join_game/0,
      fun game_overview/0
     ]}.

login() ->
    ?assertEqual(login_exp_data(),
                 web_parser:parse(login_data())).

get_session_user() ->
    ?assertEqual(get_session_user_exp_data(),
                 web_parser:parse(get_session_user_data())).

register() ->
    ?assertEqual(register_exp_data(),
                 web_parser:parse(register_data())).

update() ->
    ?assertEqual(update_exp_data(),
                 web_parser:parse(update_data())).

get_game() ->
    ?assertEqual(get_game_exp_data(),
                 web_parser:parse(get_game_data())).

create_game() ->
    ?assertEqual(create_game_exp_data(),
                 web_parser:parse(create_game_data())).

reconfig_game() ->
    ?assertEqual(reconfig_game_exp_data(),
                 web_parser:parse(reconfig_game_data())).

join_game() ->
    ?assertEqual(join_game_exp_data(),
                 web_parser:parse(join_game_data())).

game_overview() ->
    ?assertEqual(game_overview_exp_data(),
                 web_parser:parse(game_overview_data())).


%% Expected data
login_exp_data() ->
   {login,
    {ok, #user{nick = "maximus",
               password = "hunter2"}}}.

register_exp_data() ->
   {register,
    {ok, #user{nick = "maximus",
               password = "hunter2",
               email = "maximus@hotmail.com",
               name = "Maximus Decimus Meridius"}}}.

update_exp_data() ->
   {update_user,
    {ok,
     "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
     [{#user.password, "hunter2"},
      {#user.email, "maximus@hotmail.com"},
      {#user.name, "Maximus Decimus Meridius"}]}}.

create_game_exp_data() ->
    {create_game,
     {ok,
      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
      #game{name = "War of the world",
            press = "grey",
            password = "hunter2",
            order_phase = 10,
            retreat_phase = 10,
            build_phase = 10,
            waiting_time = 10,
            num_players = 7,
            creator_id = undefined}}}.

get_session_user_exp_data() ->
    {get_session_user, {ok, "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
                        dummy}}.

get_game_exp_data() ->
    {get_game, {ok, "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==", 654321}}.

reconfig_game_exp_data() ->
    {reconfig_game,
     {ok,
      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
      {654321,
      [{#game.name, "War of worlds"},
       {#game.press,  "white"},
       {#game.order_phase, 120},
       {#game.retreat_phase, 240},
       {#game.build_phase, 360},
       {#game.waiting_time, 420},
       {#game.description, "Game description"},
       {#game.num_players, 7},
       {#game.password, "pass"},
       {#game.creator_id, field_missing}]}}}.

join_game_exp_data() ->
    {join_game, {ok,
                 "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
                 {654321, germany}}}.

game_overview_exp_data() ->
    {game_overview, {ok,
                     "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==", 654321}}.

%% Input data
login_data() ->
    {ok,{struct,
     [{"action","login"},
      {"data",
       {array, [{struct,[{"nick","maximus"}]},
                {struct,[{"password","hunter2"}]}]}}]}}.

register_data() ->
    {ok,{struct,
     [{"action","register"},
      {"data",
       {array,
           [{struct,[{"email","maximus@hotmail.com"}]},
            {struct,[{"name","Maximus Decimus Meridius"}]},
            {struct,[{"nick","maximus"}]},
            {struct,[{"password","hunter2"}]}]}}]}}.


update_data() ->
    {ok,{struct,
     [{"action","update_user"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
            {struct,[{"email","maximus@hotmail.com"}]},
            {struct,[{"name","Maximus Decimus Meridius"}]},
            {struct,[{"password","hunter2"}]}]}}]}}.

create_game_data() ->
    {ok,{struct,
     [{"action","create_game"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
            {struct,[{"name","War of the world"}]},
            {struct,[{"description",""}]},
            {struct,[{"press","grey"}]},
            {struct,[{"password","hunter2"}]},
            {struct,[{"order_phase","10"}]},
            {struct,[{"retreat_phase","10"}]},
            {struct,[{"build_phase","10"}]},
            {struct,[{"waiting_time","10"}]},
            {struct,[{"num_players","7"}]}]}}]}}.

get_session_user_data() ->
    {ok,{struct,
     [{"action","get_session_user"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]}]}}]}}.

get_game_data() ->
    {ok,{struct,
     [{"action","get_game"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
            {struct,[{"game_id","654321"}]}]}}]}}.

reconfig_game_data() ->
    {ok,{struct,
     [{"action","reconfig_game"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
            {struct,[{"game_id","654321"}]},
            {struct,[{"name","War of worlds"}]},
            {struct,[{"description","Game description"}]},
            {struct,[{"press","white"}]},
            {struct,[{"password","pass"}]},
            {struct,[{"order_phase","120"}]},
            {struct,[{"retreat_phase","240"}]},
            {struct,[{"build_phase","360"}]},
            {struct,[{"waiting_time","420"}]},
            {struct,[{"num_players","7"}]}]}}]}}.

join_game_data() ->
    {ok,{struct,
     [{"action","join_game"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
            {struct,[{"country","germany"}]},
            {struct,[{"game_id","654321"}]}]}}]}}.

game_overview_data() ->
    {ok,{struct,
     [{"action","game_overview"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
            {struct,[{"game_id","654321"}]}]}}]}}.