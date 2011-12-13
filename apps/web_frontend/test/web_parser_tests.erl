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
-include_lib("datatypes/include/message.hrl").
-include_lib("datatypes/include/game.hrl").

-export([]).

parser_test_() ->
    {inorder,
     [
      fun parse_countries_str/0,
      fun game_msg/0,
      fun user_msg/0,
      fun login/0,
      fun register/0,
      fun update/0,
      fun get_game/0,
      fun create_game/0,
      fun get_session_user/0,
      fun reconfig_game/0,
      fun join_game/0,
      fun game_overview/0,
      fun game_order/0,
      fun games_current/0,
      fun get_db_stats/0,
      fun assign_moderator/0,
      fun power_msg/0,
      fun get_system_status/0,
      fun game_search/0,
      fun get_games_ongoing/0,
      fun operator_get_game_msg/0,
      fun operator_game_overview/0,
      fun stop_game/0,
      fun get_presence/0
     ]}.

get_db_stats() ->
    ?assertEqual(get_db_stats_exp_data(), web_parser:parse(get_db_stats_data())).

get_system_status() ->
    ?assertEqual(get_system_status_exp_data(), web_parser:parse(get_system_status_data())).

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

game_order() ->
    ?assertEqual(game_order_exp_data(),
                 web_parser:parse(game_order_data())).

games_current() ->
    ?assertEqual(games_current_exp_data(),
                 web_parser:parse(games_current_data())).

game_search() ->
    ?assertEqual(game_search_exp_data(),
                 web_parser:parse(game_search_data())).

user_msg() ->
    ?assertEqual (user_msg_exp_data(),
                  web_parser:parse(user_msg_data())).

game_msg() ->
    ?assertEqual (game_msg_exp_data(),
                  web_parser:parse(game_msg_data())).

stop_game() ->
    ?assertEqual(stop_game_exp_data(),
                 web_parser:parse(stop_game_data())).

power_msg() ->
    ?assertEqual (power_msg_exp_data(),
                  web_parser:parse(power_msg_data())).

assign_moderator() ->
    ?assertEqual (assign_moderator_exp_data(),
                  web_parser:parse(assign_moderator_data())).

parse_countries_str() ->
    ?assertEqual ([england, germany, france],
                  web_parser:parse_countries_str({array,["england",
                                                         "germany",
                                                         "france"]})).

get_games_ongoing() ->
    ?assertEqual (get_games_ongoing_exp_data(),
                  web_parser:parse(get_games_ongoing_data())).

get_presence() ->
    ?assertEqual (get_presence_exp_data(),
                  web_parser:parse(get_presence_data())).

operator_get_game_msg() ->
    ?assertEqual (operator_get_game_msg_exp_data(),
                  web_parser:parse(operator_get_game_msg_data())).

operator_game_overview() ->
    ?assertEqual (operator_game_overview_exp_data(),
                  web_parser:parse(operator_game_overview_data())).

%% Expected data
operator_get_game_msg_exp_data() ->
    {operator_get_game_msg,
     {ok,"g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
      {"order_key","query"}}}.

operator_game_overview_exp_data() ->
    {operator_game_overview,
                   {ok,"g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
                       654321}}.

get_db_stats_exp_data() ->
    {get_db_stats,
     {ok,"g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",[]}}.

get_system_status_exp_data() ->
    {get_system_status,
                   {ok,"g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}}.

game_msg_exp_data() ->
    {game_msg,
     {ok,
      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
      #frontend_msg{game_id = 1234,
                    to = [england,germany],
                    content = "hi!"}}}.

user_msg_exp_data() ->
    {user_msg,
     {ok,
      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
      #frontend_msg{to = "maximus",
                    content = "hi!"}}}.

power_msg_exp_data() ->
    {power_msg,
     {ok,
      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
      #frontend_msg{game_id = 1234,
                    to = [england,germany],
                    content = "hi!"}}}.

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
            description = "",
            press = grey,
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
       {#game.press,  white},
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

game_order_exp_data() ->
    {game_order, {ok,"g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
                  {654321,
                   [{move,army,london,norwegian_sea,any_coast},
                    {move,army,london,norwegian_sea,north_coast},
                    {move,army,london,norwegian_sea,any_coast},
                    {move,army,london,norwegian_sea,any_coast}]}}}.

games_current_exp_data() ->
    {games_current, {ok,
                     "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==", dummy}}.

game_search_exp_data() ->
    {game_search,{ok,"g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
                  "name=War of the world AND "
                  "description=The world of war AND "
                  "press=grey AND "
                  "status=ongoing AND "
                  "order_phase=10 AND "
                  "retreat_phase=10 AND "
                  "build_phase=10 AND "
                  "waiting_time=10 AND "
                  "num_players=7"}}.
assign_moderator_exp_data() ->
    {assign_moderator,
                   {ok,"g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==",
                       {"nick",remove}}}.

get_games_ongoing_exp_data() ->
    {get_games_ongoing, {ok,
                     "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==", dummy}}.

stop_game_exp_data() ->
    {stop_game, {ok,
                     "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==", 654321}}.

get_presence_exp_data() ->
    {get_presence, {ok,
                     "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg==", "qnick"}}.

%% Input data

get_db_stats_data() ->
    {ok,{struct,
     [{"action","get_db_stats"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]}]}}]}}.

get_system_status_data() ->
    {ok,{struct,
     [{"action","get_system_status"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]}]}}]}}.

assign_moderator_data() ->
    {ok, {struct,
          [{"action","assign_moderator"},
           {"data",
            {array,
             [{struct,[{"session_id",
                        "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
              {struct,[{"nick","nick"}]},
              {struct,[{"is_moderator","remove"}]}]}}]}}.

game_msg_data() ->
    {ok, {struct,
      [{"action", "game_msg"},
       {"data",
        {array,
         [{struct,[{"session_id",
                    "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
          {struct, [{"to", {array,
                            ["england", "germany"]}}]},
          {struct, [{"game_id", "1234"}]},
          {struct, [{"content", "hi!"}]}]}}]}}.

user_msg_data() ->
    {ok, {struct,
      [{"action", "user_msg"},
       {"data",
        {array,
         [{struct,[{"session_id",
                    "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
          {struct, [{"to", "maximus"}]},
          {struct, [{"content", "hi!"}]}]}}]}}.

power_msg_data() ->
    {ok, {struct,
      [{"action", "power_msg"},
       {"data",
        {array,
         [{struct,[{"session_id",
                    "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
          {struct, [{"to", {array,
                            ["england", "germany"]}}]},
          {struct, [{"game_id", "1234"}]},
          {struct, [{"content", "hi!"}]}]}}]}}.

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

game_order_data() ->
    {ok,{struct,
     [{"action","game_order"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
            {struct,[{"game_id","654321"}]},
            {struct,[{"game_order", "A Lon-Nrg\r\nA Lon-Nrg\r\nA Lon -> Nrg nc\r\nArmy Lon move Nrg"}]}]}}]}}.

games_current_data() ->
    {ok,{struct,
     [{"action","games_current"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]}]}}]}}.

game_search_data() ->
    {ok,{struct,
     [{"action","game_search"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
            {struct,[{"name","War of the world"}]},
            {struct,[{"description","The world of war"}]},
            {struct,[{"press","grey"}]},
            {struct,[{"status","ongoing"}]},
            {struct,[{"order_phase","10"}]},
            {struct,[{"retreat_phase","10"}]},
            {struct,[{"build_phase","10"}]},
            {struct,[{"waiting_time","10"}]},
            {struct,[{"num_players","7"}]}]}}]}}.

get_games_ongoing_data() ->
    {ok,{struct,
     [{"action","get_games_ongoing"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]}]}}]}}.

stop_game_data() ->
    {ok,{struct,
     [{"action","stop_game"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
            {struct,[{"game_id","654321"}]}]}}]}}.

get_presence_data() ->
    {ok,{struct,
     [{"action","get_presence"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
            {struct,[{"nick","qnick"}]}]}}]}}.

operator_game_overview_data() ->
    {ok,{struct,
     [{"action","operator_game_overview"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
            {struct,[{"game_id","654321"}]}]}}]}}.

operator_get_game_msg_data() ->
    {ok,{struct,
     [{"action","operator_get_game_msg"},
      {"data",
       {array,
           [{struct,[{"session_id",
                      "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA4gAAAAAAg=="}]},
            {struct,[{"query","query"}]},
            {struct,[{"order_key","order_key"}]}]}}]}}.
