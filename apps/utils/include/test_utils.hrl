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

% Dummyland is not a valid country
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

-define(SAMPLE_TEST_ORDERS1,
<<"
    sddfaadfaff

    ORDER

    SESSION: g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==
    GAMEID: 3958230945903

------move----------------
    A Lon-Nrg
    Lon-Nrg
    A Lon -> Nrg nc
    Army Lon move Nrg

---short-convoy-----------
    F Nth C A Lon-Nrg
    F Nth Convoy A Lon move Nrg

---njudge-convoy-move-----
    A Bre-Fin-Wes-Gol-Mar
    A Bre->Fin->Wes->Gol->Mar

------long convoy---------
    A Bre-Mar
    F Fin C A Bre-Mar
    F Wes C A Bre-Mar
    F Gol C A Bre-Mar

    Army Bre-> Mar
    F Fin C A Bre move Mar
    F Wes Convoy A Bre->Mar
    Fleet Gol C A Bre - Mar


------hold----------------
    A Bre H
    Army Bre hold
    hol hold

------support hold--------
    F Fin S Bre
    Fleet Fin support Bre

------support move--------
    A MUN S F KIE-BER
    Army MUN Support Fleet KIE move BER

------retreat-------------
    A Mun disband

------build---------------
    remove A mun
    build F Mun nc
    waive, Fleet Mid_Atlantic_Ocean move North_Atlantic_Ocean nc

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
    Lon-Nrg
    A Lon -> Nrg nc
    Army Lon move Nrg

    F Nth C A Lon-Nrg
    F Nth Convoy A Lon move Nrg

    END
    2563564565asdfa
">>
).