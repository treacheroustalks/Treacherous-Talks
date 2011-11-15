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
%%%=======================================================================
%%%
%%% Based on examples at http://bobpeers.com/technical/telnet_imap
%%%
%%% What: An IMAP client
%%% Purpose: For testing that an expected message was received.
%%%
%%% NB: This really isn't a general purpose IMAP client!
%%%
%%%=======================================================================

-module(imap_client).

-export([read_first/1, empty_mailbox/1]).

%% Read mail number 1 in the main mailbox of Username
%%
%% email() -> string()    e.g. test@dilshod.pcs
%% username() -> email()
%% from() -> "From: " ++ email()
%% to() -> "To: " ++ email()
%% subject() -> "Subject: ..."
%% read_first(username()) -> {ok, {from(), to(), subject(), body()}}
%%
read_first(Username) ->
%    debug(Username),
    {ok, Sock} = connect("mail.pcs"),
    login(Sock, Username, "password"),
    select(Sock),
    case read_one(Sock, 1, 5) of
        {ok, FirstMail} ->
            Return = {ok, FirstMail};
        Error ->
            Return = Error
    end,
    logout(Sock),
    Return.

%% Delete all messages in the main mailbox of Username
empty_mailbox(Username) ->
    {ok, Sock} = connect("mail.pcs"),
    login(Sock, Username, "password"),
    select(Sock),
    delete_all(Sock),
    logout(Sock),
    ok.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%read_one(Sock, MsgNum, Retries)
read_one(_Sock, _MsgNum, 0) ->
    {error, out_of_retries};
read_one(Sock, MsgNum, Retries) ->
    case catch read_one(Sock, MsgNum) of
        {ok, Good} ->
            Good;
        {'EXIT', {Reason, _Stack}} ->
            io:format("Reading mail failed. Retrying in 500ms... ~p~n",[Reason]),
            timer:sleep(500),
            select(Sock),
            read_one(Sock, MsgNum, Retries-1)
    end.

% succeeds or crashes.
read_one(Sock, MsgNum) ->
    MsgNumStr = lists:flatten(io_lib:format("~p",[MsgNum])),
    {ok, From} = fetch(Sock, MsgNumStr ++ " (body[header.fields (from)])"),
    {ok, To} = fetch(Sock, MsgNumStr ++ " (body[header.fields (to)])"),
    {ok, Subject} = fetch(Sock, MsgNumStr ++ " (body[header.fields (subject)])"),
    {ok, Body} = fetch(Sock, MsgNumStr ++ " (body[text])"),
    {ok, {From, To, Subject, Body}}.


connect(Hostname) ->
    {ok, Sock} = gen_tcp:connect(Hostname, 143,
                                 [binary,
                                  {packet, 0},
                                  {active, false}]),
    {ok, _Packet} = recv(Sock),
    {ok, Sock}.


login(Sock, Username, Password) ->
    ok = gen_tcp:send(Sock, ". login " ++ Username ++ " "
                      ++ Password ++ "\r\n"),
    {ok, _Packet} = recv(Sock).


select(Sock) ->
    ok = gen_tcp:send(Sock, ". select INBOX\r\n"),
    {ok, _Packet} = recv(Sock).


fetch(Sock, Args) ->
    ok = gen_tcp:send(Sock, ". fetch " ++ Args ++ "\r\n"),
    {ok, Packet} = recv(Sock),
    case Packet of
        <<". BAD Error in IMAP command FETCH: Invalid messageset\r\n">> ->
            {error, "Requested mail doesn't exist."};
        _ ->
            Response = binary_to_list(Packet),
            Lines = string:tokens(Response, "\n"),
            Result = lists:nth(2,Lines),
            Trimmed = string:substr(Result, 1, string:str(Result, "\r")-1),
            {ok, Trimmed}
    end.


logout(Sock) ->
    ok = gen_tcp:send(Sock, ". logout\r\n"),
    {ok, _Packet} = recv(Sock).


%% Marks everything in the selected folder as ready to delete,
%% then PERMANENTLY DELETES THEM!!
delete_all(Sock) ->
    ok = gen_tcp:send(Sock, ". store 1:* flags \\Deleted\r\n"),
    recv(Sock),
    ok = gen_tcp:send(Sock, ". expunge\r\n"),
    recv(Sock).

recv(Sock) ->
    gen_tcp:recv(Sock, 0, 5000).


%debug(Val) ->
%    io:format("~p~n", [Val]).
