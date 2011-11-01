%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
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
    [get_header([]), get_simple_page(Page), get_footer([])].

%% Load yaws page using server side includes - http://yaws.hyber.org/ssi.yaws
get_simple_page(Page) ->
    {yssi, "page/"++Page++".yaws"}.

get_header(Bindings) ->
    {ssi, "page/header.html","%%", Bindings}.

get_footer(Bindings) ->
    {ssi, "page/footer.html","%%", Bindings}.