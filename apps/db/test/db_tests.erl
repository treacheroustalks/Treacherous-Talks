-module(db_tests).

-include_lib("eunit/include/eunit.hrl").
-include ("include/user.hrl").

riak_running_test () ->
    Ip = "127.0.0.1",
    Port = 8081,
    case riakc_pb_socket:start (Ip, Port) of
    {ok, Pid} ->
        riakc_pb_socket:stop (Pid);
    Error ->
        erlang:error ({error,
               riak_not_running,
               {Error,
                "riak has to run on 127.0.0.1, pb_port 8081"}})
    end.

db_started_setup () ->
    db:start_link ().

db_started_teardown (_) ->
    catch db:stop ().

crud_on_db_test_ () ->
    {"writing, reading, deleting and reading test users",
     {setup,
      fun db_started_setup/0,
      fun db_started_teardown/1,
      crud_on_db ()}}.

crud_on_db () ->
    [{"adding john & martha", fun () ->
                                      ?debugVal (db:john ()),
                                      ?debugVal (db:martha ()),

                                      {ok, JID} = db:add_user (db:john ()),
                                      [JohnDB] = db:get_user (JID),
                                      ?debugVal (JID),
                                      ?debugVal (JohnDB),
                                      JohnDB = db:john (),
                                      ok = db:delete_user (JID),
                                      [unknown] = db:get_user (JID),
                                      {ok, MID} = db:add_user (db:martha ()),
                                      [MarthaDB] = db:get_user (MID),
                                      ?debugVal (MID),
                                      ?debugVal (MarthaDB),
                                      MarthaDB = db:martha (),
                                      ok = db:delete_user (MID),
                                      [unknown] = db:get_user (MID)
                end}].

%% Suku: this is commented out, since I am working on it right now 
%% and it's not ready yet
%link_test_disable () ->
%    {"setting and reading a bunch of links",
%     {foreach,
%      fun db_started_setup/0,
%      fun db_started_teardown/1,
%      write_links_on_db ()
%      ++ read_links_on_db ()
%     }}.

%write_links_on_db () ->
%    [fun () ->
%             ?debugMsg (link_on_db),
%             {ok, JID} = db:add_user (db:john ()),
%             {ok, MID} = db:add_user (db:martha ()),
%             db:add_link (<<"friend">>,
%                          {<<"user">>, JID},
%                          {<<"user">>, MID}),
%             {ok, JID, MID}
%     end].

%read_links_on_db () ->
%    [fun () ->
%             ?debugMsg (read_links_on_db),
%             {ok, JID} = db:add_user (db:john ()),
%             {ok, MID} = db:add_user (db:martha ()),
%             db:add_link (<<"friend">>,
%                          {<<"user">>, JID},
%                          {<<"user">>, MID}),
%             ?debugVal (JID),
%             [Martha]=db:get_user (MID),
%             [Martha]=db:follow_link (<<"friend">>, <<"user">>, JID)
%     end].
