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
-define(SUPPORTED_COMMANDS,
            ?REGISTER ++ "," ++
            ?LOGIN ++ "," ++
            ?UPDATE ++ "," ++
            ?RECONFIG ++ "," ++
            ?CREATE ++ "," ++
            ?JOIN ++ "," ++
            ?ORDER ++ "," ++
            ?OVERVIEW ++ "," ++
            ?VIEWCURRENTGAMES ++ "," ++
            ?SEARCH ++ "," ++
            ?GETPROFILE).
-define(REGISTER, "REGISTER").
-define(UPDATE, "UPDATE").
-define(LOGIN, "LOGIN").
-define(RECONFIG, "RECONFIG").
-define(CREATE, "CREATE").
-define(OVERVIEW, "OVERVIEW").
-define(VIEWCURRENTGAMES, "VIEWCURRENTGAMES").
-define(JOIN, "JOIN").
-define(ORDER, "ORDER").
-define(SEARCH, "SEARCH").
-define(GETPROFILE, "GETPROFILE").
-define(END, "END").


-define(SESSION, "SESSION").
-define(NICKNAME, "NICKNAME").
-define(PASSWORD, "PASSWORD").
-define(FULLNAME, "FULLNAME").
-define(EMAIL, "EMAIL").
-define(CHANNEL, "CHANNEL").
-define(ROLE, "ROLE").
-define(DATE_CREATED, "DATE_CREATED").


-define(GAMEID, "GAMEID").
-define(GAMENAME, "GAMENAME").
-define(PRESSTYPE, "PRESSTYPE").
-define(DESCRIPTION, "DESCRIPTION").
-define(NUMBEROFPLAYERS, "NUMBEROFPLAYERS").
-define(ORDERCIRCLE, "ORDERCIRCLE").
-define(RETREATCIRCLE, "RETREATCIRCLE").
-define(GAINLOSTCIRCLE, "GAINLOSTCIRCLE").
-define(WAITTIME, "WAITTIME").
-define(COUNTRY, "COUNTRY").
-define(STATUS, "STATUS").
-define(RESULT, "RESULT").

-define(SUPPLYCENTER, "SUPPLY CENTERS").

% countries allowed in the game
-define(COUNTRIES, [england,
                    germany,
                    france,
                    austria,
                    italy,
                    russia,
                    turkey]).



-define(MESSAGE, "MESSAGE").
-define(POWERMESSAGE, "POWERMESSAGE").
-define(CONTENT, "CONTENT").
-define(TO, "TO").
-define(VALID_PRESS, [white, grey, none]).
-define(COUNTRY_LOOKUP,
[{"austria", austria},
 {"england", england},
 {"france", france},
 {"italy", italy},
 {"russia", russia},
 {"germany", germany},
 {"turkey", turkey},
 {"a", austria},
 {"e", england},
 {"f", france},
 {"g", germany},
 {"i", italy},
 {"r", russia},
 {"t", turkey}
]).
