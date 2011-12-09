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
-include_lib("datatypes/include/push_receiver.hrl").


apps () ->
    [datatypes, service, protobuffs, riakc, db,
     mnesia, controller_app, message].

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
             from_id = user_id1(),
             to_nick = "valid_nick",
             to_id = to_user_id(),
             content = "hi"
            }.
test_msg2() ->
    #message{from_nick = "dave",
             from_id = user_id2(),
             to_nick = "valid_nick",
             to_id = to_user_id(),
             content = "hello"
            }.
test_game_msg(ToUser) ->
    #game_message{from_country = germany,
                  game_id = 123456,
             from_id = user_id1(),
             to_country = france,
             to_id = ToUser,
             content = "game message"
            }.

test_key() ->
    1234.
user_id1() ->
    5555.
user_id2() ->
    4444.

to_user_id() ->
    2222.

to_user() ->
    #user{id = to_user_id(), nick = "valid_nick", email="sth@sth.pcs"}.
to_user2() ->
    #user{id = 3333, nick = "valid_nick2", email="sthelse@sth.pcs"}.

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
    [{"write a user message in db",
      fun() ->
        message_worker:log_message(test_key(), test_msg(), ?B_MESSAGE),
        timer:sleep(100),
        ActualValue = message_util:get_message(test_key(), ?B_MESSAGE, message),
        ?assertEqual({ok, test_msg()},ActualValue),
        db:delete(?B_MESSAGE, db:int_to_bin(test_key()))
     end},
     {"write a game message in db",
      fun() ->
        message_worker:log_message(test_key(), test_game_msg(1122), ?B_GAME_MESSAGE),
        timer:sleep(100),
        ActualValue = message_util:get_message(test_key(), ?B_GAME_MESSAGE,
                                               game_message),
        ?assertEqual({ok, test_game_msg(1122)},ActualValue),
        db:delete(?B_GAME_MESSAGE, db:int_to_bin(test_key()))
     end}
     ].

message_fail_tst_() ->
    [fun() ->
             Msg = test_msg(),
             Reply = message:user_msg(Msg#message{to_nick = "invalid nick" ++
                                            integer_to_list(db_c:get_unique_id())}),
             ?assertEqual({error,invalid_nick}, Reply)
     end].

message_success_tst_() ->
    [fun() ->
             % register to_user
             user_management:create(to_user()),

             % send msg to "to_user"
             {ok, Key} = message:user_msg(test_msg()),

             timer:sleep(100),
             % check if the msg has been correctly stored in db
             {ok, ActualMessage} = message_util:get_message(Key, ?B_MESSAGE, message),
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
               timer:sleep(100),

               % check if the msg has been correctly stored in db
               Result = message:unread((to_user())#user.id),

               {ok, {UnreadUserMsg, []}} = Result,

               % exactly as many as expected, ie NOT OtherMessage
               ?assertEqual(2, length(UnreadUserMsg)),

               % cheat: copy in the data instead of mecking
               Actual1 = lists:keyfind(Key1, #message.id, UnreadUserMsg),

               Expected1 = (test_msg())#message{id = Key1,
                                                date_created = Actual1#message.date_created,
                                                status = unread},

               Actual2 = lists:keyfind(Key2, #message.id, UnreadUserMsg),
               Expected2 = (test_msg2())#message{id = Key2,
                                                 date_created = Actual2#message.date_created,
                                                 status = unread},
               % exactly as expected
               ?assertEqual(Expected1, Actual1),
               ?assertEqual(Expected2, Actual2)
       end},
      {"All user and game message of given user retrieved, and they contain what they should",
       fun() -> % test
               % send msg to "to_user"
               {ok, Key1} = message:user_msg(test_msg()),
               {ok, Key2} = message:user_msg(test_msg2()),

               % send game message to "to_user"
               ok = message:game_msg(test_game_msg(to_user_id())),

               % send message to a different user. We don't expect to see this in
               % the result of message:unread/1
               OtherMessage = ( test_msg2() )#message{ to_id = (to_user2())#user.id,
                                                       to_nick = (to_user2())#user.nick },
               {ok, _Key3} = message:user_msg(OtherMessage),
               % game messsage for another user
               GameMsg2 = (test_game_msg(user_id2()))#game_message{
                                                          to_country = england},
               ok = message:game_msg(GameMsg2),

               % check if the msg has been correctly stored in db
               Result = message:unread((to_user())#user.id),

               {ok, {UnreadUserMsges, UnreadGameMsges}} = Result,

               % exactly as many as expected, ie NOT OtherMessage
               ?assertEqual(2, length(UnreadUserMsges)),
               ?assertEqual(1, length(UnreadGameMsges)),

               % cheat: copy in the data instead of mecking
               Actual1 = lists:keyfind(Key1, #message.id, UnreadUserMsges),

               Expected1 = (test_msg())#message{id = Key1,
                                                date_created = Actual1#message.date_created,
                                                status = unread},

               Actual2 = lists:keyfind(Key2, #message.id, UnreadUserMsges),
               Expected2 = (test_msg2())#message{id = Key2,
                                                 date_created = Actual2#message.date_created,
                                                 status = unread},
               [GameMsg] = UnreadGameMsges,
               Expected3 = (test_game_msg(to_user_id()))#game_message{
                                                    id =GameMsg#game_message.id},

               % exactly as expected
               ?assertEqual(Expected1, Actual1),
               ?assertEqual(Expected2, Actual2),
               ?assertEqual(Expected3, GameMsg)

       end},
      {"user and game messages are retrieved",
       fun() -> % test
               % send msg to "to_user"
               {ok, Key1} = message:user_msg(test_msg()),
               {ok, _Key2} = message:user_msg(test_msg2()),

               % send game message to "to_user"
               ok = message:game_msg(test_game_msg(to_user_id())),
               GameMsg2 = (test_game_msg(to_user_id()))#game_message{
                                                          from_country = england,
                                                          from_id = user_id2()},
               ok = message:game_msg(GameMsg2),

               % mark one of each game and user message as read
               message:mark_user_msg_as_read(Key1),
               make_game_msg_as_read(to_user_id(), user_id2()),

               % exactly one is now unread
               {ok, {[UnreadUserMsg], [UnreadGameMsg]}} = message:unread((to_user())#user.id),

               ?assertEqual((test_msg2())#message.content, UnreadUserMsg#message.content),
               Expected = (test_game_msg(to_user_id()))#game_message.from_country,
               ?assertEqual(Expected,
                            UnreadGameMsg#game_message.from_country)
       end},
      {"message:mark_as_unread/2 positive test",
       fun() ->
               %check for message bucket
               {ok, Key1} = message:user_msg(test_msg()),
               timer:sleep(100),
               {ok, Message} = message_util:get_message(Key1, ?B_MESSAGE, message),
               ?assertEqual(unread, Message#message.status),
               ok = message:mark_user_msg_as_read(Key1),
               timer:sleep(100),
               {ok, ReadMessage} = message_util:get_message(Key1, ?B_MESSAGE, message),
               ?assertEqual(read, ReadMessage#message.status),

               %check for game_message bucket
               ok = message:game_msg(test_game_msg(to_user_id())),
               timer:sleep(100),
               Result= message:unread((to_user())#user.id),
               ?debugFmt("it should return unread game messages ~p~n", [Result]),
               {ok, {[], [UnreadGameMsg]}} =Result,
               GameMsgKey = UnreadGameMsg#game_message.id,
               {ok, GMsg} = message_util:get_message(GameMsgKey,
                                                        ?B_GAME_MESSAGE,
                                                        game_message),
               ?assertEqual(unread, GMsg#game_message.status),

               ok = message:mark_game_msg_as_read(GameMsgKey),
               timer:sleep(100),
               {ok, ReadGMsg} = message_util:get_message(GameMsgKey,
                                                        ?B_GAME_MESSAGE,
                                                        game_message),
               ?assertEqual(read, ReadGMsg#game_message.status)
       end},
      {"message:mark_as_unread/1 negative test",
       fun() ->
               % Make sure it doesn't exist
               Key = 12345,
               db:delete(?B_MESSAGE, db:int_to_bin(Key)),
               ?assertEqual({error, notfound},
                            message:mark_user_msg_as_read(Key))
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
    delete_messages(UserId,?B_MESSAGE),
    delete_messages(UserId,?B_GAME_MESSAGE).

delete_messages(UserId,Bucket) ->
    Query = io_lib:format("to_id=~p", [UserId]),
    {ok, Result} = db:search(Bucket, Query),
    Keys = data_format:search_result_keys(Result),
    lists:map(fun(Key) ->
                      db:delete(Bucket, db:int_to_bin(Key))
              end,
              Keys).

make_game_msg_as_read(ToUserId, FromUserId) ->
    {ok, {_, UnreadGameMsges}} = message:unread(ToUserId),
    GMsg = lists:keyfind(FromUserId, #game_message.from_id, UnreadGameMsges),
    ?debugVal(GMsg),
    R = message:mark_game_msg_as_read(GMsg#game_message.id),
    timer:sleep(100),
    R.
