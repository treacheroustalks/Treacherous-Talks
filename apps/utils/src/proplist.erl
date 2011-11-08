%%%-------------------------------------------------------------------
%%% @doc proplist
%%%
%%% A module for our property list utilities.
%%%
%%% @end
%%--------------------------------------------------------------------

-module(proplist).

-export([update_record/2]).

%%------------------------------------------------------------------------------
%% @doc update record via a proplist
%%  Input: Arg1: OldUser#user
%%         Arg2: [{#user.name, "username"}, {#user.password, "xxxx"}]
%%
%%  Output: #user{name="username", password="xxxx"}
%% @end
%%------------------------------------------------------------------------------
update_record(Old, [{_, field_missing}|Rest]) ->
    update_record(Old, Rest);
update_record(Old, [{Field, Value}|Rest]) ->
    update_record(setelement(Field, Old, Value), Rest);
update_record(Updated, []) ->
    Updated.
