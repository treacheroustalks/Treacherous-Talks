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


parse_login_test_() ->
    ActualOutput = web_parser:parse(get_login_data()),
    Expected = {login,
                {ok, #user{nick = "maximus",
                           password = "hunter2"}}},
    ?_assertEqual(Expected, ActualOutput).


parse_register_test_() ->
    ActualOutput = web_parser:parse(get_register_data()),
    Expected = {register,
                {ok, #user{nick = "maximus",
                           password = "hunter2",
                           email = "maximus@hotmail.com",
                           name = "Maximus Decimus Meridius"}}},
    ?_assertEqual(Expected, ActualOutput).


parse_update_test_() ->
    ActualOutput = web_parser:parse(get_update_data()),
    Expected = {update_user,
                {ok,
                 123456,
                 [{#user.password, "hunter2"},
                  {#user.email, "maximus@hotmail.com"},
                  {#user.name, "Maximus Decimus Meridius"}]}},
    ?_assertEqual(Expected, ActualOutput).


parse_create_game_test_() ->
    ActualOutput = web_parser:parse(get_create_game_data()),
    Expected = {create_game,
             {ok,
              123456,
              #game{name = "War of the world",
                    press = "grey",
                    password = "hunter2",
                    order_phase = 10,
                    retreat_phase = 10,
                    build_phase = 10,
                    waiting_time = 10,
                    creator_id = undefined}}},
    ?_assertEqual(Expected, ActualOutput).


parse_get_session_user_test_() ->
    ActualOutput = web_parser:parse(get_get_session_user_data()),
    Expected = {get_session_user, {ok, 123456}},
    ?_assertEqual(Expected, ActualOutput).


get_login_data() ->
    {ok,{struct,
    [{"action","login"},
                  {"data",
                   {array, [{struct,[{"nick","maximus"}]},
                            {struct,[{"password","hunter2"}]}]}}]}}.

get_register_data() ->
    {ok,{struct,
     [{"action","register"},
      {"data",
       {array,
           [{struct,[{"email","maximus@hotmail.com"}]},
            {struct,[{"fullname","Maximus Decimus Meridius"}]},
            {struct,[{"nick","maximus"}]},
            {struct,[{"password","hunter2"}]}]}}]}}.


get_update_data() ->
    {ok,{struct,
     [{"action","update_user"},
      {"data",
       {array,
           [{struct,[{"session_id","123456"}]},
            {struct,[{"email","maximus@hotmail.com"}]},
            {struct,[{"fullname","Maximus Decimus Meridius"}]},
            {struct,[{"password","hunter2"}]}]}}]}}.

get_create_game_data() ->
    {ok,{struct,
     [{"action","create_game"},
      {"data",
       {array,
           [{struct,[{"session_id","123456"}]},
            {struct,[{"name","War of the world"}]},
            {struct,[{"description",""}]},
            {struct,[{"press","grey"}]},
            {struct,[{"password","hunter2"}]},
            {struct,[{"order_phase","10"}]},
            {struct,[{"retreat_phase","10"}]},
            {struct,[{"build_phase","10"}]},
            {struct,[{"order_phase","10"}]},
            {struct,[{"waiting_time","10"}]},
            {struct,[{"fullname","Maximus Decimus Meridius"}]}]}}]}}.

get_get_session_user_data() ->
    {ok,{struct,
     [{"action","get_session_user"},
      {"data",
       {array,
           [{struct,[{"session_id","123456"}]}]}}]}}.
