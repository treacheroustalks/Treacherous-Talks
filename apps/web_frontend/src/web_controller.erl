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
%%% @doc Appmod for Yaws
%%%      Module handles all requests sent to Yaws as mentioned in yaws
%%%      config
%%% @end
%%%
%%% @since : 25 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(web_controller).

-include_lib("yaws/include/yaws_api.hrl").

-export([out/1]).

%%-------------------------------------------------------------------
%% @doc
%% Yaws function that gets called for all requests as per yaws config
%% @end
%%-------------------------------------------------------------------
out(A) ->
    % Tokenize URL and handle it based on URI
    case string:tokens(A#arg.appmoddata, "/") of
        % Default landing page
        [] ->
            get_page("index");
        % Web socket endpoint
        ["endpoint"] ->
            get_simple_page("endpoint");
        % Load all static yaws pages
        [Page] ->
            get_page(Page);
        _ ->
        % @todo Set 404 headers
        get_page("404")
    end.


get_page(Page) ->
    [get_header([]), get_simple_page(Page), get_chat ([]), get_footer([])].

%% Load yaws page using server side includes - http://yaws.hyber.org/ssi.yaws
get_simple_page(Page) ->
    {yssi, "page/"++Page++".yaws"}.

get_chat (Bindings) ->
    {ssi, "page/chat.html", "%%", Bindings}.

get_header(Bindings) ->
    {ssi, "page/header.html","%%", Bindings}.

get_footer(Bindings) ->
    {ssi, "page/footer.html","%%", Bindings}.
