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
    [adr, aeg, alb, ank, apu, arm, bal, bar, bel, ber, bla, boh, bre, bud, bul,
     bur, cly, con, den, eas, edi, eng, fin, gal, gas, gre, bot, gol, hel, hol,
     ion, iri, kie, lvp, lvn, lon, mar, mid, mos, mun, nap, naf, nat, nth, nwy,
     nrg, par, pic, pie, por, pru, rom, ruh, rum, ser, sev, sil, ska, smy, spa,
     stp, swe, syr, tri, tun, tus, tyr, tyn, ukr, ven, vie, wal, war, wes, yor]).
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

-define(TRANS_LOC_FULLNAME,
[{"adriatic_sea", adr},
 {"aegean_sea", aeg},
 {"albania", alb},
 {"ankara", ank},
 {"apulia", apu},
 {"armenia", arm},
 {"baltic_sea", bal},
 {"barents_sea", bar},
 {"belgium", bel},
 {"berlin", ber},
 {"black_sea", bla},
 {"bohemia", boh},
 {"brest", bre},
 {"budapest", bud},
 {"bulgaria", bul},
 {"burgundy", bur},
 {"clyde", cly},
 {"constantinople", con},
 {"denmark", den},
 {"eastern_mediterranean", eas},
 {"edinburgh", edi},
 {"english_channel", eng},
 {"finland", fin},
 {"galicia", gal},
 {"gascony", gas},
 {"greece", gre},
 {"gulf_of_bothnia", bot},
 {"gulf_of_lyon", gol},
 {"helgoland_bight", hel},
 {"holland", hol},
 {"ionian_sea", ion},
 {"irish_sea", iri},
 {"kiel", kie},
 {"liverpool", lvp},
 {"livonia", lvn},
 {"london", lon},
 {"marseilles", mar},
 {"mid_atlantic_ocean", mid},
 {"moscow", mos},
 {"munich", mun},
 {"naples", nap},
 {"north_africa", naf},
 {"north_atlantic_ocean", nat},
 {"north_sea", nth},
 {"norway", nwy},
 {"norwegian_sea", nrg},
 {"paris", par},
 {"picardy", pic},
 {"piedmont", pie},
 {"portugal", por},
 {"prussia", pru},
 {"rome", rom},
 {"ruhr", ruh},
 {"rumania", rum},
 {"serbia", ser},
 {"sevastopol", sev},
 {"silesia", sil},
 {"skagerrak", ska},
 {"smyrna", smy},
 {"spain", spa},
 {"st_petersburg", stp},
 {"sweden", swe},
 {"syria", syr},
 {"trieste", tri},
 {"tunis", tun},
 {"tuscany", tus},
 {"tyrolia", tyr},
 {"tyrrhenian_sea", tyn},
 {"ukraine", ukr},
 {"venice", ven},
 {"vienna", vie},
 {"wales", wal},
 {"warsaw", war},
 {"western_mediterranean", wes},
 {"yorkshire", yor}]
).

-endif.