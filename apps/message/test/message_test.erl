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
%%% @doc Tests of message API
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
    meck:new(controller),
    meck:expect(controller, push_event, 2, ok),
    ?debugMsg (io_lib:format ("~p", [Response])).

app_started_teardown (_) ->
    % shut up info message spew
    error_logger:tty(false),
    [application:stop (App) || App <- lists:reverse (apps ())],
    meck:unload(controller).

test_msg() ->
    #message{from_nick = "bob",
             from_id = 5555,
             to_nick = "valid_nick",
             to_id = 2222,
             content = "hi"
            }.
test_msg2() ->
    #message{from_nick = "dave",
             from_id = 4444,
             to_nick = "valid_nick",
             to_id = 2222,
             content = "hello"
            }.

test_key() ->
    1234.

to_user_id() ->
    2222.

to_user() ->
    #user{id = to_user_id(), nick = "valid_nick", email="sth@sth.pcs"}.
to_user2() ->
    #user{id = 3333, nick = "valid_nick2", email="sthelse@sth.pcs"}.

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
      message_success_tst_(),
      unread_tst_()
     ]}.

ping_tst_ () ->
    [fun()-> {pong, _Pid} = message_worker:ping () end].

message_worker_tst_() ->
    [fun() ->
        message_worker:log_user_msg(test_key(), test_msg()),
        DBObj = get_DB_obj(?B_MESSAGE, test_key()),
        ?assertEqual(expected_link_value(), db_obj:get_links(DBObj)),
        ActualValue = message_util:get_message(test_key()),
        ?assertEqual({ok, test_msg()},ActualValue),
        db:delete(?B_MESSAGE, db:int_to_bin(test_key()))
     end].

message_fail_tst_() ->
    [fun() ->
             Msg = test_msg(),
             Reply = message:user_msg(Msg#message{to_nick = "invalid nick"}),
             ?assertEqual({error,invalid_nick}, Reply)
     end].

message_success_tst_() ->
    [fun() ->
             % register to_user
             user_management:create(to_user()),

             % send msg to "to_user"
             {ok, Key} = message:user_msg(test_msg()),

             % check if links are correct
             DBObj = get_DB_obj(?B_MESSAGE, Key),
             ?assertEqual(expected_link_value(), db_obj:get_links(DBObj)),

             % check if the msg has been correctly stored in db
             {ok, ActualMessage} = message_util:get_message(Key),
             Expected = (test_msg())#message{id = Key,
                                           date_created =
                                               ActualMessage#message.date_created},
             ?assertEqual(Expected,
                          ActualMessage),

             % clean up
             db:delete(?B_USER, db:int_to_bin(to_user_id())),
             db:delete(?B_MESSAGE, db:int_to_bin(Key))
     end].

unread_tst_() ->
    {foreach,
     fun() -> % setup
             % register tested and other user
             user_management:create(to_user()),
             user_management:create(to_user2()),

             % DB doesn't provide consistency. Ensure no existing messages.
             delete_user_messages(( to_user() )#user.id),
             delete_user_messages(( to_user2() )#user.id)
     end,
     fun(_) -> % cleanup
             db:delete(?B_USER, db:int_to_bin(to_user_id())),
             db:delete(?B_USER, db:int_to_bin(( to_user2() )#user.id)),
             delete_user_messages(( to_user() )#user.id),
             delete_user_messages(( to_user2() )#user.id)
     end,
     [{"Only messages of given user retrieved, and they contain what they should",
       fun() -> % test
               % send msg to "to_user"
               {ok, Key1} = message:user_msg(test_msg()),
               {ok, Key2} = message:user_msg(test_msg2()),

               % send message to a different user. We don't expect to see this in
               % the result of message:unread/1
               OtherMessage = ( test_msg2() )#message{ to_id = (to_user2())#user.id,
                                                       to_nick = (to_user2())#user.nick },
               {ok, _Key3} = message:user_msg(OtherMessage),

               % check if the msg has been correctly stored in db
               Result = message:unread((to_user())#user.id),

               {ok, Unread} = Result,

               % exactly as many as expected, ie NOT OtherMessage
               ?assertEqual(2, length(Unread)),

               % cheat: copy in the data instead of mecking
               Actual1 = lists:keyfind(Key1, #message.id, Unread),

               Expected1 = (test_msg())#message{id = Key1,
                                                date_created = Actual1#message.date_created,
                                                status = unread},

               Actual2 = lists:keyfind(Key2, #message.id, Unread),
               Expected2 = (test_msg2())#message{id = Key2,
                                                 date_created = Actual2#message.date_created,
                                                 status = unread},
               % exactly as expected
               ?assertEqual(Expected1, Actual1),
               ?assertEqual(Expected2, Actual2)
       end},
      {"Only unread messages are retrieved",
       fun() -> % test
               % send msg to "to_user"
               {ok, Key1} = message:user_msg(test_msg()),
               {ok, _Key2} = message:user_msg(test_msg2()),

               % mark one as read
               message:mark_as_read(Key1),

               % exactly one is now unread
               {ok, [Unread]} = message:unread((to_user())#user.id),

               ?assertEqual((test_msg2())#message.content, Unread#message.content)
       end},
      {"message:mark_as_unread/1 positive test",
       fun() ->
               {ok, Key1} = message:user_msg(test_msg()),
               {ok, Message} = message_util:get_message(Key1),
               ?assertEqual(unread, Message#message.status),

               ok = message:mark_as_read(Key1),
               {ok, ReadMessage} = message_util:get_message(Key1),
               ?assertEqual(read, ReadMessage#message.status)
       end},
      {"message:mark_as_unread/1 negative test",
       fun() ->
               % Make sure it doesn't exist
               Key = 12345,
               db:delete(?B_MESSAGE, db:int_to_bin(Key)),
               ?assertEqual({error, notfound}, message:mark_as_read(Key))
       end}
     ]}.


get_DB_obj(Bucket, Key) ->
    BinKey = db:int_to_bin(Key),
    DBReply = db:get(Bucket, BinKey),
    case DBReply of
        {ok, DBObj} ->
            DBObj;
        Other ->
            Other
    end.

delete_user_messages(UserId) ->
    Query = io_lib:format("to_id=~p", [UserId]),
    {ok, Result} = db:search(?B_MESSAGE, Query),
    Keys = data_format:search_result_keys(Result),
    lists:map(fun(Key) ->
                      db:delete(?B_MESSAGE, db:int_to_bin(Key))
              end,
              Keys).
