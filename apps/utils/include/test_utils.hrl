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
-define(SESSION_ID, "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==").
-define(GAME_ID_VAL, "11112222").

%%--------------------------------------------------------------------
%% sample XMPP messages
%%--------------------------------------------------------------------

-define(SAMPLE_REGISTER,
<<"sddfaadfaff\r
REGISTER\r
NICKNAME: Lin\r
PASSWORD: QWER\r
\r
EMAIL: ss@lin.pcs\r
asdfasdlfadf\r
\r
FULLNAME: Agner Erlang\r
END\r
adfadfasdfaldfad">>
).

-define(SAMPLE_REGISTER_CHANNEL,
<<"sddfaadfaff\r
REGISTER\r
NICKNAME: Lin\r
PASSWORD: QWER\r
\r
CHANNEL: mail \r
EMAIL: ss@lin.pcs\r
asdfasdlfadf\r
\r
FULLNAME: Agner Erlang\r
END\r
adfadfasdfaldfad">>
).

-define(SAMPLE_LOGIN,
<<"sddfaadfaff\r
LOGIN\r
NICKNAME: Lin\r
PASSWORD: QWER\r
END\r
adfadfasdfaldfad">>
).

-define(SAMPLE_UPDATE,
<<"sddfaadfaff\r
UPDATE\r
SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==\r
PASSWORD: QWER\r
FULLNAME: Agner Erlang\r
END\r
adfadfasdfaldfad">>
).

-define(SAMPLE_CREATE,
<<"sddfaadfaff\r
CREATE\r
----Required Fields---------\r
SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==\r
GAMENAME: awesome_game\r
PRESSTYPE: white\r
ORDERCIRCLE: 4H\r
RETREATCIRCLE: 3H30M\r
GAINLOSTCIRCLE: 2H40M\r
WAITTIME: 2D5H20M\r
----Optional Fields---------\r
PASSWORD: 1234
\r
END\r
adfadfasdfaldfad">>
).

-define(SAMPLE_RECONFIG_WITHOUT_GAMEID,
<<"sddfaadfaff\r
RECONFIG\r
----Required Fields---------\r
GAMENAME: awesome_game\r
PRESSTYPE: white\r
ORDERCIRCLE: 4H\r
RETREATCIRCLE: 3H30M\r
GAINLOSTCIRCLE: 2H40M\r
WAITTIME: 2D5H20M\r
----Optional Fields---------\r
PASSWORD: 1234
\r
END\r
adfadfasdfaldfad">>
).

-define(SAMPLE_RECONFIG,
<<"sddfaadfaff\r
RECONFIG\r
----Required Fields---------\r
SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==\r
GAMEID: 111222  \r
GAMENAME: awesome_game\r
PRESSTYPE: white\r
ORDERCIRCLE: 4H\r
RETREATCIRCLE: 3H30M\r
GAINLOSTCIRCLE: 2H40M\r
WAITTIME: 2D5H20M\r
----Optional Fields---------\r
PASSWORD: 1234
\r
END\r
adfadfasdfaldfad">>
).

-define(SAMPLE_RECONFIG_2,
<<"sddfaadfaff\r
RECONFIG\r
----Required Fields---------\r
SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==\r
GAMEID: 111222  \r
GAMENAME: awesome_game\r
PRESSTYPE: white\r
ORDERCIRCLE: 4H\r
----Optional Fields---------\r
PASSWORD: 1234
\r
END\r
adfadfasdfaldfad">>
).
-define(SAMPLE_RECONFIG_WITH_CHARGID,
<<"sddfaadfaff\r
RECONFIG\r
----Required Fields---------\r
SESSION: asd54-asd \r
GAMEID: aws \r
GAMENAME: awesome_game\r
PRESSTYPE: white\r
ORDERCIRCLE: 4H\r
RETREATCIRCLE: 3H30M\r
GAINLOSTCIRCLE: 2H40M\r
WAITTIME: 2D5H20M\r
----Optional Fields---------\r
PASSWORD: 1234
\r
END\r
adfadfasdfaldfad">>
).


-define(SAMPLE_GAME_OVERVIEW,
<<"sddfaadfaff\r
OVERVIEW\r
----Required Fields---------\r
SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==\r
GAMEID: 111222 \r
\r
END\r
adfadfasdfaldfad">>
).
%% View current games ----------------------------------
-define(SAMPLE_GAMES_CURRENT(Session),
<<"
    asdfasdfasdfasdf
    SEARCH
    SESSION: " Session "
    END
    2563564565asdfa
">>
).
%% Join game ------------------------------------------
-define(SAMPLE_JOIN_GAME,
<<"sddfaadfaff\r
JOIN\r
----Required Fields---------\r
SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==\r
GAMEID: 111222 \r
COUNTRY: England
\r
END\r
adfadfasdfaldfad">>
).

% Dummyland is not a valid country --------------------
-define(SAMPLE_JOIN_GAME_ERROR,
<<"sddfaadfaff\r
JOIN\r
----Required Fields---------\r
SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==\r
GAMEID: 111222 \r
COUNTRY: Dummyland
\r
END\r
adfadfasdfaldfad">>
).
%% Game search ----------------------------------------
-define(SAMPLE_GAME_SEARCH(Session),
<<"
    asdfasdfasdfasdf
    SEARCH
    SESSION: " Session "
    GAMEID: 1234
    GAMENAME: simple_game
    WAITTIME: 1D
    END
    2563564565asdfa
">>
).

-define(SAMPLE_TEST_ORDERS1,
<<"
    sddfaadfaff

    ORDER

    SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==
    GAMEID: 3958230945903

    A Lon-Nrg
    A Belgium m Mun
    A Lon -> Nrg nc
    Army Lon move Nrg
    Army Bre->Mar
    Fleet Mid_Atlantic_Ocean move North_Atlantic_Ocean nc
    A Bre-Mar

    F Nth C A Lon-Nrg
    F Nth Convoy A Lon move Nrg
    F Nth Convoy A yor-nwy
    F Wes C A Bre-Mar
    F Fin C A Bre move Mar
    F Wes Convoy A Bre->Mar
    Fleet Gol C A Bre - Mar

    A munich H
    Army Bre hold
    a hol hold

    F Fin S A Bre
    F marseilles S A albania
    Fleet Fin support A Bre
    A MUN S F KIE-BER
    Army   MUN   Support   Fleet   KIE   move   BER
    Army   MUN   Support   A   belgium   m   BER

    disband A Mun
    D A mun

    build F Mun nc
    B F Mun nc


    END

    adfadfasdfaldfad">>
).

-define(SAMPLE_TEST_ORDERS_ID_SESSION,
<<"  ORDER

    SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==
    GAMEID: 3958230945903

------move----------------
    A Lon-Nrg

    END

    adfadfasdfaldfad">>
).

-define(SAMPLE_TEST_ORDERS2,
<<"
    asdfasdfasdfasdf
    ORDER

    SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==
    GAMEID: 3958230945903

    A Lon-Nrg
    A Lon m Nrg
    A Lon -> Nrg nc
    Army Lon move Nrg

    F Nth C A Lon-Nrg
    F Nth Convoy A Lon move Nrg

    END
    2563564565asdfa
">>
).

-define(SAMPLE_TEST_ORDERS_UNIT_MISSING,
<<"
    asdfasdfasdfasdf
    ORDER

    SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==
    GAMEID: 3958230945903

    Lon-Nrg
    A Lon m Nrg
    A Lon -> Nrg nc
    Army Lon move Nrg

    F Nth C A Lon-Nrg
    F Nth Convoy A Lon move Nrg

    END
    2563564565asdfa
">>
).
-define(SAMPLE_TEST_ORDERS_ACTION_MISSING,
<<"
    asdfasdfasdfasdf
    ORDER

    SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==
    GAMEID: 3958230945903

    A Nrg
    A Lon -> Nrg nc
    Army Lon move Nrg

    F Nth C A Lon-Nrg
    F Nth Convoy A Lon move Nrg

    END
    2563564565asdfa
">>
).

-define(SAMPLE_USER_MSG(Session),
<<"
    asdfasdfasdfasdf
    MESSAGE

    SESSION: " Session "
    TO: nick
    CONTENT:

    A sample message to nick player which
    contain several line
    have fun

    END
    2563564565asdfa
">>
).

-define(SAMPLE_USER_MSG_WRONG,
<<"
    asdfasdfasdfasdf
    MESSAGE

    SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==
    TO: nick
    END
    2563564565asdfa
">>
).

-define(SAMPLE_GAME_MSG(Session, GameID),
<<"
    asdfasdfasdfasdf
    MESSAGE

    SESSION: " Session "
    TO: england
    GAMEID: " GameID "
    CONTENT:

    A sample message to nick player which
    contain several line
    have fun

    END
    2563564565asdfa
">>
).

-define(SAMPLE_GAME_MSG_WRONG(Session, GameID),
<<"
    asdfasdfasdfasdf
    MESSAGE

    TO: england
    GAMEID: " GameID "
    CONTENT:

    A sample message to nick player which
    contain several line
    have fun

    END
    2563564565asdfa
">>
).

-define(SAMPLE_GAME_MSG_MULTICOUNTRY(Session, GameID),
<<"
    asdfasdfasdfasdf
    MESSAGE

    SESSION: " Session "
    TO: germany england russia austria
    GAMEID: " GameID "
    CONTENT:

    A sample message to nick player which
    contain several line
    have fun

    END
    2563564565asdfa
">>
).
-define(SAMPLE_GAME_MSG_NO_TO(Session, GameID),
<<"
    asdfasdfasdfasdf
    MESSAGE

    SESSION: " Session "
    GAMEID: " GameID "
    CONTENT:

    A sample message to nick player which
    contain several line
    have fun

    END
    2563564565asdfa
">>
).

-define(SAMPLE_POWER_MSG(Session, GameID),
<<"
    asdfasdfasdfasdf
    POWERMESSAGE

    SESSION: " Session "
    TO: england
    GAMEID: " GameID "
    CONTENT:

    A sample message to nick player which
    contain several line
    have fun

    END
    2563564565asdfa
">>
).

-define(SAMPLE_GET_PRESENCE(Session),
<<"
    asdfasdfasdfasdf
    GETPRESENCE

    SESSION: " Session "
    NICKNAME: testusernick

    END
    2563564565asdfa
">>
).

-define(SAMPLE_GET_PROFILE(Session),
<<"
    asdfasdfasdfasdf
    GETPROFILE

    SESSION: " Session "

    END
    2563564565asdfa
">>
).


%%--------------------------------------------------------------------
%% test utilities
%%--------------------------------------------------------------------

-define(NOW_UNIV, calendar:now_to_universal_time(os:timestamp())).
