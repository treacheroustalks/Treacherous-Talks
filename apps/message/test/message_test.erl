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
%%% @author A.Rahim Kadkhodamohammadi <r.k.mohammadi@gmail.com>
%%%
%%% @doc Unit tests for updating user
%%% @end
%%%
%%% @since : 15 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(message_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/bucket.hrl").
-include_lib("datatypes/include/message.hrl").


apps () ->
    [datatypes, service, protobuffs, riakc, db, message].

app_started_setup () ->
    ?debugMsg ("starting apps:"),
    Response = [{App, application:start (App)} || App <- apps ()],
    ?debugMsg (io_lib:format ("~p", [Response])).

app_started_teardown (_) ->
    [application:stop (App) || App <- lists:reverse (apps ())].

test_msg() ->
    #message{from = 5555, to = 2222}.

test_key() ->
    1234.

to_user_id() ->
    2222.

to_user() ->
    #user{id = to_user_id(), nick = "valid_nick", email="sth@sth.pcs"}.

expected_link_value() ->
    [{{<<"user">>,db:int_to_bin(to_user_id())},<<"to_user">>},
     {{<<"user">>,<<"5555">>},<<"from_user">>}].

%%------------------------------------------------------------------------------
%% @doc
%%  the top level test
%% @end
%%------------------------------------------------------------------------------
move_get_put_test_ () ->
    {setup,
     fun app_started_setup/0,
     fun app_started_teardown/1,
     [ping_tst_ (),
      message_worker_tst_(),
      message_fail_tst_(),
      message_success_tst_()
     ]}.

ping_tst_ () ->
    [fun()-> {pong, _Pid} = message_worker:ping () end].

message_worker_tst_() ->
    [fun() ->
        message_worker:log_user_msg(test_key(), test_msg()),
        DBObj = get_DB_obj(?B_MESSAGE, test_key()),
        ?assertEqual(expected_link_value(), db_obj:get_links(DBObj)),
        ActualValue = get_DB_obj_val(DBObj),
        ?assertEqual({ok, test_msg()},ActualValue),
        db:delete(?B_MESSAGE, db:int_to_bin(test_key()))
     end].

message_fail_tst_() ->
    [fun() ->
        Reply = message:user_msg("invalid_user", test_msg()),
        ?assertEqual({error,invalid_nick}, Reply)
     end].

message_success_tst_() ->
    [fun() ->
        % register to_user
        user_management:create(to_user()),

        % send msg to "to_user"
        Result = message:user_msg("valid_nick", test_msg()),
        {ok, Key} = Result,
        db:delete(?B_USER, db:int_to_bin(to_user_id())),

        % check if links are correct
        DBObj = get_DB_obj(?B_MESSAGE, Key),
        ?assertEqual(expected_link_value(), db_obj:get_links(DBObj)),

        % check if the msg has been correctly stored in db
        ActualValue = get_DB_obj_val(DBObj),
        {ok, Val} = ActualValue,
        Msg = test_msg(),
        ?assertEqual({ok, Msg#message{id = Key,
                      date_created = Val#message.date_created}}, ActualValue),
        db:delete(?B_MESSAGE, db:int_to_bin(Key))
     end].

get_DB_obj(Bucket, Key) ->
    BinKey = db:int_to_bin(Key),
    DBReply = db:get(Bucket, BinKey),
    case DBReply of
        {ok, DBObj} ->
            DBObj;
        Other ->
            Other
    end.

get_DB_obj_val(DBObj) ->
    {ok, db_obj:get_value(DBObj)}.