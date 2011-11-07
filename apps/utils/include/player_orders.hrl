-ifndef(PLAYER_ORDERS).
-define(PLAYER_ORDERS, true).

-define(ORD_PARSER_SETTING, [{capture,[1, 2, 3, 4, 5, 7, 8] ,list}]).
-define(UNIT_PTN, "(army|fleet|a|f)?").
-define(LOC_PTN, "([a-z_]{3,})?").
-define(ACT_PTN, "(->|-|remove|move|support|hold$|convoy|disband$|build|waive|m|s|h$|c|d$|b|r|w$)").
-define(MOV_PTN, "(->|-|move|m)?").
-define(COAST_PTN, "([ens]c)?").
-define(ORD_PARSER,
"\s*"?UNIT_PTN"\s*"?LOC_PTN"\s*"?ACT_PTN"\s*"?UNIT_PTN"\s*"?LOC_PTN"\s*"?MOV_PTN"\s*"?LOC_PTN"\s*"?COAST_PTN"\s*").

-define(LOCATIONS,
    [adriatic_sea,aegean_sea,albania,ankara,apulia,armenia,baltic_sea,
     barents_sea,belgium,berlin,black_sea,bohemia,brest,budapest,bulgaria,
     burgundy,clyde,constantinople,denmark,eastern_mediterranean,edinburgh,
     english_channel,finland,galicia,gascony,greece,gulf_of_bothnia,
     gulf_of_lyon,helgoland_bight,holland,ionian_sea,irish_sea,kiel,liverpool,
     livonia,london,marseilles,mid_atlantic_ocean,moscow,munich,naples,
     north_africa,north_atlantic_ocean,north_sea,norway,norwegian_sea,paris,
     picardy,piedmont,portugal,prussia,rome,ruhr,rumania,serbia,sevastopol,
     silesia,skagerrak,smyrna,spain,st_petersburg,sweden,syria,trieste,tunis,
     tuscany,tyrolia,tyrrhenian_sea,ukraine,venice,vienna,wales,warsaw,
     western_mediterranean,yorkshire]).
%---------------------------------
% subj short for subject
% obj short for object
% loc short for location
% src short for source location
% dst short for destination
%---------------------------------
% Move Example:
% A Mun M Vie => move#{subj_unit=army, subj_src_loc=mun, subj_dst_loc=vie, coast=nil}
-record(move, {subj_unit, subj_src_loc, subj_dst_loc, coast}).

% Hold Example:
% F Adr H => hold#{subj_unit=fleet, subj_loc=adr}
-record(hold, {subj_unit, subj_loc}).

% Support Move Example:
% A Vie S A Boh M Gal => support_move#{subj_unit=army, subj_loc=vie, obj_unit=army,
% obj_src_loc=boh, obj_dst_loc=gal}
-record(support_move, {subj_unit, subj_loc, obj_unit, obj_src_loc, obj_dst_loc, coast}).

% Support Hold Example:
% A Vie S A Boh => support_hold#{subj_unit=army, subj_loc=vie, obj_unit=army,
% obj_loc=boh}
-record(support_hold, {subj_unit, subj_loc, obj_unit, obj_loc}).

% F Adr C A Apu M Tri => convoy#{subj_unit=fleet, subj_loc=adr, obj_unit=army,
% obj_src_loc=apu, obj_dst_loc=tri, coast=nil}
-record(convoy, {subj_unit, subj_loc, obj_unit, obj_src_loc, obj_dst_loc}).

-record(disband, {subj_unit, subj_loc}).
-record(build, {obj_unit, obj_loc, coast}).
-record(remove, {obj_unit, obj_loc}).
-record(waive, {}).

-define(TRANS_COAST,
[{"nc", north_coast},
 {"sc", south_coast},
 {"ec", east_coast},
 {[], any_coast}]
).

-define(TRANS_UNIT,
[{"a", army},
 {"f", fleet},
 {"army", army},
 {"fleet", fleet},
 {[], any_unit}]
).

-define(TRANS_ACTION,
[{"->", move},
 {"-", move},
 {"m", move},
 {"h", hold},
 {"s", support},
 {"c", convoy},
 {"d", disband},
 {"r", remove},
 {"b", build},
 {"w", waive},
 {"move", move},
 {"hold", hold},
 {"support", support},
 {"convoy", convoy},
 {"disband", disband},
 {"remove", remove},
 {"build", build},
 {"waive", waive}]
).

-define(TRANS_LOC_ABBV,
[{"adr", adriatic_sea},
 {"aeg", aegean_sea},
 {"alb", albania},
 {"ank", ankara},
 {"apu", apulia},
 {"arm", armenia},
 {"bal", baltic_sea},
 {"bar", barents_sea},
 {"bel", belgium},
 {"ber", berlin},
 {"bla", black_sea},
 {"boh", bohemia},
 {"bre", brest},
 {"bud", budapest},
 {"bul", bulgaria},
 {"bur", burgundy},
 {"cly", clyde},
 {"con", constantinople},
 {"den", denmark},
 {"eas", eastern_mediterranean},
 {"edi", edinburgh},
 {"eng", english_channel},
 {"fin", finland},
 {"gal", galicia},
 {"gas", gascony},
 {"gre", greece},
 {"bot", gulf_of_bothnia},
 {"gol", gulf_of_lyon},
 {"hel", helgoland_bight},
 {"hol", holland},
 {"ion", ionian_sea},
 {"iri", irish_sea},
 {"kie", kiel},
 {"lvp", liverpool},
 {"lvn", livonia},
 {"lon", london},
 {"mar", marseilles},
 {"mid", mid_atlantic_ocean},
 {"mos", moscow},
 {"mun", munich},
 {"nap", naples},
 {"naf", north_africa},
 {"nat", north_atlantic_ocean},
 {"nth", north_sea},
 {"nwy", norway},
 {"nrg", norwegian_sea},
 {"par", paris},
 {"pic", picardy},
 {"pie", piedmont},
 {"por", portugal},
 {"pru", prussia},
 {"rom", rome},
 {"ruh", ruhr},
 {"rum", rumania},
 {"ser", serbia},
 {"sev", sevastopol},
 {"sil", silesia},
 {"ska", skagerrak},
 {"smy", smyrna},
 {"spa", spain},
 {"stp", st_petersburg},
 {"swe", sweden},
 {"syr", syria},
 {"tri", trieste},
 {"tun", tunis},
 {"tus", tuscany},
 {"tyr", tyrolia},
 {"tyn", tyrrhenian_sea},
 {"ukr", ukraine},
 {"ven", venice},
 {"vie", vienna},
 {"wal", wales},
 {"war", warsaw},
 {"wes", western_mediterranean},
 {"yor", yorkshire}
]).

-endif.