var unit_arr = new Array();//[unit, country, province]
var orderCache;
var selectedUnit;
var isMyUnit;
var supportedUnits;//[supportOrConvoy, yourSupportingUnit]
var canvas;
var context;
var buildNum;
var disbandNum;
var centerCount;
var mapLoaded = false;
var mouseDrag = false;
var canvasWidth = 1154;
var canvasHeight = 996;
var mA = [0,0];
var mOX, mOY;
var mapDataImage = new Image();
var mapFGImage = new Image();
var mapBGImage = new Image();
var coloredBG;
var loadedMap = 0;
var colorLayerData;
var provLookUp;
var puLookUp;
var pixelStack = new Array();
var newColorR;
var newColorG;
var newColorB;
var newColorA;
var clickedColorR;
var clickedColorG;
var clickedColorB;
var clickedColorA;
var drawingAreaX = 0;
var drawingAreaY = 0;
var drawingAreaWidth = 1154;
var drawingAreaHeight = 996;
var paths = new Array();
var provArea = ["//////","baltic_sea","barents_sea","gulf_of_bothnia",
    "helgoland_bight","skagerrak","irish_sea","english_channel","north_sea",
    "norwegian_sea","north_atlantic_ocean","mid_atlantic_ocean",
    "western_mediterranean","gulf_of_lyon","tyrrhenian_sea","adriatic_sea",
    "ionian_sea","aegean_sea","black_sea","eastern_mediterranean","sweden",
    "norway","denmark","holland","belgium","spain","portugal","north_africa",
    "tunis","albania","serbia","greece","bulgaria","rumania","trieste","bohemia",
    "galicia","vienna","tyrolia","budapest","clyde","edinburgh","liverpool",
    "yorkshire","wales","london","picardy","brest","paris","burgundy","gascony",
    "marseilles","piedmont","tuscany","rome","naples","apulia","venice","kiel",
    "berlin","prussia","ruhr","munich","silesia","livonia","finland",
    "st_petersburg","warsaw","ukraine","moscow","sevastopol","armenia","ankara",
    "constantinople","smyrna","syria"];

var coords = {baltic_sea:[620,438],barents_sea:[845,45],
    gulf_of_bothnia:[659,327],helgoland_bight:[465,443],skagerrak:[511,376],
    irish_sea:[223,478],english_channel:[297,529],north_sea:[390,375],
    norwegian_sea:[450,116],north_atlantic_ocean:[173,248],
    mid_atlantic_ocean:[90,625],western_mediterranean:[307,857],
    gulf_of_lyon:[369,792],tyrrhenian_sea:[491,844],adriatic_sea:[577,780],
    ionian_sea:[607,947],aegean_sea:[759,917],black_sea:[908,739],
    eastern_mediterranean:[865,962],sweden:[596,268],norway:[522,280],
    denmark:[521,429],holland:[435,506],belgium:[411,546],spain:[198,764],
    portugal:[102,741],north_africa:[243,939],tunis:[426,947],albania:[649,824],
    serbia:[670,769],greece:[686,867],bulgaria:[741,782],rumania:[774,720],
    trieste:[598,726],bohemia:[568,595],galicia:[710,609],vienna:[603,634],
    tyrolia:[536,649],budapest:[677,672],clyde:[321,337],edinburgh:[342,354],
    liverpool:[325,417],yorkshire:[349,438],wales:[306,477],london:[340,493],
    picardy:[365,566],brest:[297,584],paris:[352,618],burgundy:[407,630],
    gascony:[309,681],marseilles:[384,703],piedmont:[450,698],tuscany:[493,765],
    rome:[514,795],naples:[564,844],apulia:[577,826],venice:[492,702],
    kiel:[492,491],berlin:[554,495],prussia:[638,493],ruhr:[463,539],
    munich:[498,588],silesia:[600,552],livonia:[723,422],finland:[735,219],
    st_petersburg:[892,273],warsaw:[679,549],ukraine:[789,584],moscow:[906,453],
    sevastopol:[1020,606],armenia:[1073,823],ankara:[930,823],
    constantinople:[819,840],smyrna:[902,894],syria:[1069,930]};

var prov_adj = {yorkshire:
[[1,"wales"],[2,"north_sea"],[3,"london"],[1,"liverpool"],[3,"edinburgh"]],western_mediterranean:
[[2,"tyrrhenian_sea"],[2,"tunis"],[2,"spain"],[2,"north_africa"],[2,"mid_atlantic_ocean"],[2,"gulf_of_lyon"]],warsaw:
[[1,"ukraine"],[1,"silesia"],[1,"prussia"],[1,"moscow"],[1,"livonia"],[1,"galicia"]],wales:
[[1,"yorkshire"],[3,"london"],[3,"liverpool"],[2,"irish_sea"],[2,"english_channel"]],vienna:
[[1,"tyrolia"],[1,"trieste"],[1,"galicia"],[1,"budapest"],[1,"bohemia"]],venice:
[[1,"tyrolia"],[3,"tuscany"],[3,"trieste"],[1,"rome"],[1,"piedmont"],[3,"apulia"],[2,"adriatic_sea"]],ukraine:
[[1,"warsaw"],[1,"sevastopol"],[1,"rumania"],[1,"moscow"],[1,"galicia"]],tyrrhenian_sea:
[[2,"western_mediterranean"],[2,"tuscany"],[2,"tunis"],[2,"rome"],[2,"naples"],[2,"ionian_sea"],[2,"gulf_of_lyon"]],tyrolia:
[[1,"vienna"],[1,"venice"],[1,"trieste"],[1,"piedmont"],[1,"munich"],[1,"bohemia"]],tuscany:
[[3,"venice"],[2,"tyrrhenian_sea"],[3,"rome"],[3,"piedmont"],[2,"gulf_of_lyon"]],tunis:
[[2,"western_mediterranean"],[2,"tyrrhenian_sea"],[3,"north_africa"],[2,"ionian_sea"]],trieste:
[[1,"vienna"],[3,"venice"],[1,"tyrolia"],[1,"serbia"],[1,"budapest"],[3,"albania"],[2,"adriatic_sea"]],syria:
[[3,"smyrna"],[1,"armenia"],[2,"eastern_mediterranean"]],sweden:
[[2,"skagerrak"],[3,"norway"],[2,"gulf_of_bothnia"],[3,"finland"],[3,"denmark"],[2,"baltic_sea"]],st_petersburg:
[[3,"norway"],[1,"moscow"],[3,"livonia"],[2,"gulf_of_bothnia"],[3,"finland"],[2,"barents_sea"]],spain:
[[2,"western_mediterranean"],[3,"portugal"],[2,"mid_atlantic_ocean"],[3,"marseilles"],[2,"gulf_of_lyon"],[3,"gascony"]],smyrna:
[[3,"syria"],[2,"eastern_mediterranean"],[3,"constantinople"],[1,"armenia"],[1,"ankara"],[2,"aegean_sea"]],skagerrak:
[[2,"sweden"],[2,"norway"],[2,"north_sea"],[2,"denmark"]],silesia:
[[1,"warsaw"],[1,"prussia"],[1,"munich"],[1,"galicia"],[1,"bohemia"],[1,"berlin"]],sevastopol:
[[1,"ukraine"],[3,"rumania"],[1,"moscow"],[2,"black_sea"],[3,"armenia"]],serbia:
[[1,"trieste"],[1,"rumania"],[1,"greece"],[1,"bulgaria"],[1,"budapest"],[1,"albania"]],rumania:
[[1,"ukraine"],[3,"sevastopol"],[1,"serbia"],[1,"galicia"],[3,"bulgaria"],[1,"budapest"],[2,"black_sea"]],ruhr:
[[1,"munich"],[1,"kiel"],[1,"holland"],[1,"burgundy"],[1,"belgium"]],rome:
[[1,"venice"],[2,"tyrrhenian_sea"],[3,"tuscany"],[3,"naples"],[1,"apulia"]],prussia:
[[1,"warsaw"],[1,"silesia"],[3,"livonia"],[3,"berlin"],[2,"baltic_sea"]],portugal:
[[3,"spain"],[2,"mid_atlantic_ocean"]],piedmont:
[[1,"venice"],[1,"tyrolia"],[3,"tuscany"],[3,"marseilles"],[2,"gulf_of_lyon"]],picardy:
[[1,"paris"],[2,"english_channel"],[1,"burgundy"],[3,"brest"],[3,"belgium"]],paris:
[[1,"picardy"],[1,"gascony"],[1,"burgundy"],[1,"brest"]],norwegian_sea:
[[2,"norway"],[2,"north_sea"],[2,"north_atlantic_ocean"],[2,"edinburgh"],[2,"clyde"],[2,"barents_sea"]],norway:
[[3,"sweden"],[3,"st_petersburg"],[2,"skagerrak"],[2,"norwegian_sea"],[2,"north_sea"],[1,"finland"],[2,"barents_sea"]],north_sea:
[[2,"yorkshire"],[2,"skagerrak"],[2,"norwegian_sea"],[2,"norway"],[2,"london"],[2,"holland"],[2,"helgoland_bight"],[2,"english_channel"],[2,"edinburgh"],[2,"denmark"],[2,"belgium"]],north_atlantic_ocean:
[[2,"norwegian_sea"],[2,"mid_atlantic_ocean"],[2,"liverpool"],[2,"irish_sea"],[2,"clyde"]],north_africa:
[[2,"western_mediterranean"],[3,"tunis"],[2,"mid_atlantic_ocean"]],naples:
[[2,"tyrrhenian_sea"],[3,"rome"],[2,"ionian_sea"],[3,"apulia"]],munich:
[[1,"tyrolia"],[1,"silesia"],[1,"ruhr"],[1,"kiel"],[1,"burgundy"],[1,"bohemia"],[1,"berlin"]],moscow:
[[1,"warsaw"],[1,"ukraine"],[1,"st_petersburg"],[1,"sevastopol"],[1,"livonia"]],mid_atlantic_ocean:
[[2,"western_mediterranean"],[2,"spain"],[2,"portugal"],[2,"north_atlantic_ocean"],[2,"north_africa"],[2,"irish_sea"],[2,"gascony"],[2,"english_channel"],[2,"brest"]],marseilles:
[[3,"spain"],[3,"piedmont"],[2,"gulf_of_lyon"],[1,"gascony"],[1,"burgundy"]],london:
[[3,"yorkshire"],[3,"wales"],[2,"north_sea"],[2,"english_channel"]],livonia:
[[1,"warsaw"],[3,"st_petersburg"],[3,"prussia"],[1,"moscow"],[2,"gulf_of_bothnia"],[2,"baltic_sea"]],liverpool:
[[1,"yorkshire"],[3,"wales"],[2,"north_atlantic_ocean"],[2,"irish_sea"],[1,"edinburgh"],[3,"clyde"]],kiel:
[[1,"ruhr"],[1,"munich"],[3,"holland"],[2,"helgoland_bight"],[3,"denmark"],[3,"berlin"],[2,"baltic_sea"]],irish_sea:
[[2,"wales"],[2,"north_atlantic_ocean"],[2,"mid_atlantic_ocean"],[2,"liverpool"],[2,"english_channel"]],ionian_sea:
[[2,"tyrrhenian_sea"],[2,"tunis"],[2,"naples"],[2,"greece"],[2,"eastern_mediterranean"],[2,"apulia"],[2,"albania"],[2,"aegean_sea"],[2,"adriatic_sea"]],holland:
[[1,"ruhr"],[2,"north_sea"],[3,"kiel"],[2,"helgoland_bight"],[3,"belgium"]],helgoland_bight:
[[2,"north_sea"],[2,"kiel"],[2,"holland"],[2,"denmark"]],gulf_of_lyon:
[[2,"western_mediterranean"],[2,"tyrrhenian_sea"],[2,"tuscany"],[2,"spain"],[2,"piedmont"],[2,"marseilles"]],gulf_of_bothnia:
[[2,"sweden"],[2,"st_petersburg"],[2,"livonia"],[2,"finland"],[2,"baltic_sea"]],greece:
[[1,"serbia"],[2,"ionian_sea"],[3,"bulgaria"],[3,"albania"],[2,"aegean_sea"]],gascony:
[[3,"spain"],[1,"paris"],[2,"mid_atlantic_ocean"],[1,"marseilles"],[1,"burgundy"],[3,"brest"]],galicia:
[[1,"warsaw"],[1,"vienna"],[1,"ukraine"],[1,"silesia"],[1,"rumania"],[1,"budapest"],[1,"bohemia"]],finland:
[[3,"sweden"],[3,"st_petersburg"],[1,"norway"],[2,"gulf_of_bothnia"]],english_channel:
[[2,"wales"],[2,"picardy"],[2,"north_sea"],[2,"mid_atlantic_ocean"],[2,"london"],[2,"irish_sea"],[2,"brest"],[2,"belgium"]],edinburgh:
[[3,"yorkshire"],[2,"norwegian_sea"],[2,"north_sea"],[1,"liverpool"],[3,"clyde"]],eastern_mediterranean:
[[2,"smyrna"],[2,"ionian_sea"],[2,"aegean_sea"],[2,"syria"]],denmark:
[[3,"sweden"],[2,"skagerrak"],[2,"north_sea"],[3,"kiel"],[2,"helgoland_bight"],[2,"baltic_sea"]],constantinople:
[[3,"smyrna"],[3,"bulgaria"],[2,"black_sea"],[3,"ankara"],[2,"aegean_sea"]],clyde:
[[2,"norwegian_sea"],[2,"north_atlantic_ocean"],[3,"liverpool"],[3,"edinburgh"]],burgundy:
[[1,"ruhr"],[1,"picardy"],[1,"paris"],[1,"munich"],[1,"marseilles"],[1,"gascony"],[1,"belgium"]],bulgaria:
[[1,"serbia"],[3,"rumania"],[3,"greece"],[3,"constantinople"],[2,"black_sea"],[2,"aegean_sea"]],budapest:
[[1,"vienna"],[1,"trieste"],[1,"serbia"],[1,"rumania"],[1,"galicia"]],brest:
[[3,"picardy"],[1,"paris"],[2,"mid_atlantic_ocean"],[3,"gascony"],[2,"english_channel"]],bohemia:
[[1,"vienna"],[1,"tyrolia"],[1,"silesia"],[1,"munich"],[1,"galicia"]],black_sea:
[[2,"sevastopol"],[2,"rumania"],[2,"constantinople"],[2,"bulgaria"],[2,"armenia"],[2,"ankara"]],berlin:
[[1,"silesia"],[3,"prussia"],[1,"munich"],[3,"kiel"],[2,"baltic_sea"]],belgium:
[[1,"ruhr"],[3,"picardy"],[2,"north_sea"],[3,"holland"],[2,"english_channel"],[1,"burgundy"]],barents_sea:
[[2,"st_petersburg"],[2,"norwegian_sea"],[2,"norway"]],baltic_sea:
[[2,"sweden"],[2,"prussia"],[2,"livonia"],[2,"kiel"],[2,"gulf_of_bothnia"],[2,"denmark"],[2,"berlin"]],armenia:
[[1,"syria"],[1,"smyrna"],[3,"sevastopol"],[2,"black_sea"],[3,"ankara"]],apulia:
[[3,"venice"],[1,"rome"],[3,"naples"],[2,"ionian_sea"],[2,"adriatic_sea"]],ankara:
[[1,"smyrna"],[3,"constantinople"],[2,"black_sea"],[3,"armenia"]],albania:
[[3,"trieste"],[1,"serbia"],[2,"ionian_sea"],[3,"greece"],[2,"adriatic_sea"]],aegean_sea:
[[2,"smyrna"],[2,"ionian_sea"],[2,"greece"],[2,"eastern_mediterranean"],[2,"constantinople"],[2,"bulgaria"]],adriatic_sea:
[[2,"venice"],[2,"trieste"],[2,"ionian_sea"],[2,"apulia"],[2,"albania"]]};

var totalLoadResources = 3;
var curLoadResNum = 0;

/**
 * Creates a canvas element, loads images, adds events, and draws the canvas for the first time.
 */
function prepareCanvas(units, owners, map_data)
{
    var canvas_pos = $('#canvas_div')[0];
    var coordinates = {
      //Supply centers: [flagX, flagY]
      sweden_center:           [585,305],
      norway_center:           [507,285],
      denmark_center:          [513,418],
      holland_center:          [427,514],
      belgium_center:          [413,544],
      spain_center:            [160,730],
      portugal_center:         [85,742],
      tunis_center:            [415,925],
      serbia_center:           [660,749],
      greece_center:           [690,890],
      bulgaria_center:         [723,755],
      rumania_center:          [730,714],
      budapest_center:         [670,625],
      trieste_center:          [570,690],
      vienna_center:           [595,605],
      edinburgh_center:        [325,357],
      liverpool_center:        [315,409],
      london_center:           [328,492],
      brest_center:            [296,553],
      marseilles_center:       [390,690],
      paris_center:            [362,606],
      berlin_center:           [559,494],
      kiel_center:             [497,489],
      munich_center:           [485,595],
      naples_center:           [554,828],
      rome_center:             [502,794],
      venice_center:           [489,681],
      moscow_center:           [920,400],
      sevastopol_center:       [910,685],
      st_petersburg_center:    [810,325],
      warsaw_center:           [640,514],
      ankara_center:           [899,815],
      constantinople_center:   [817,820],
      smyrna_center:           [823,882],


      //Fleet on water bodies
      baltic_sea_fleet:              [600,425],
      barents_sea_fleet:             [798,27],
      gulf_of_bothnia_fleet:         [635,353],
      helgoland_bight_fleet:         [437,440],
      skagerrak_fleet:               [470,372],
      irish_sea_fleet:               [170,480],
      english_channel_fleet:         [270,520],
      north_sea_fleet:               [390,375],
      norwegian_sea_fleet:           [390,124],
      north_atlantic_ocean_fleet:    [108,247],
      mid_atlantic_ocean_fleet:      [108,578],
      western_mediterranean_fleet:   [330,848],
      gulf_of_lyon_fleet:            [330,784],
      tyrrhenian_sea_fleet:          [470,848],
      adriatic_sea_fleet:            [565,775],
      ionian_sea_fleet:              [576,940],
      aegean_sea_fleet:              [743,925],
      black_sea_fleet:               [887,734],
      eastern_mediterranean_fleet:   [855,953],

      //Fleet on provinces
      sweden_fleet:            [587,383],
      norway_fleet:            [460,325],
      denmark_fleet:           [480,440],
      holland_fleet:           [410,478],
      belgium_fleet:           [370,517],
      spain_fleet:          [170,657],
      //spain_sc_fleet:          [128,836],
      portugal_fleet:          [85,700],
      north_africa_fleet:      [205,883],
      tunis_fleet:             [440,949],
      albania_fleet:           [627,833],
      greece_fleet:            [665,911],
      //bulgaria_sc_fleet:       [742,798],
      bulgaria_fleet:       [780,745],
      rumania_fleet:           [785,720],
      trieste_fleet:           [555,725],
      clyde_fleet:             [290,330],
      edinburgh_fleet:         [340,335],
      liverpool_fleet:         [295,390],
      yorkshire_fleet:         [338,441],
      wales_fleet:             [250,490],
      london_fleet:            [325,509],
      picardy_fleet:           [328,539],
      brest_fleet:             [255,570],
      gascony_fleet:           [256,657],
      marseilles_fleet:        [350,727],
      piedmont_fleet:          [422,710],
      tuscany_fleet:           [459,758],
      rome_fleet:              [500,800],
      naples_fleet:            [555,870],
      apulia_fleet:            [563,805],
      venice_fleet:            [490,725],
      kiel_fleet:              [453,463],
      berlin_fleet:            [530,459],
      prussia_fleet:           [605,455],
      livonia_fleet:           [670,377],
      finland_fleet:           [675,312],
      st_petersburg_fleet:  [785,280],
      //st_petersburg_nc_fleet:  [830,155],
      sevastopol_fleet:        [890,660],
      armenia_fleet:           [1025,770],
      ankara_fleet:            [890,768],
      constantinople_fleet:    [765,828],
      smyrna_fleet:            [810,917],
      syria_fleet:             [972,937],

      //Army
      sweden_army:             [567,353],
      norway_army:             [505,255],
      denmark_army:            [482,420],
      holland_army:            [423,485],
      belgium_army:            [383,533],
      spain_army:              [213,735],
      portugal_army:           [93,720],
      north_africa_army:       [243,939],
      tunis_army:              [407,949],
      albania_army:            [637,810],
      serbia_army:             [665,780],
      greece_army:             [666,845],
      bulgaria_army:           [740,770],
      rumania_army:            [758,697],
      trieste_army:            [579,708],
      bohemia_army:            [546,591],
      galicia_army:            [701,604],
      vienna_army:             [587,639],
      tyrolia_army:            [521,640],
      budapest_army:           [652,674],
      clyde_army:              [300,347],
      edinburgh_army:          [328,335],
      liverpool_army:          [310,385],
      yorkshire_army:          [337,441],
      wales_army:              [293,475],
      london_army:             [352,470],
      picardy_army:            [355,554],
      brest_army:              [287,582],
      paris_army:              [321,607],
      burgundy_army:           [390,621],
      gascony_army:            [284,675],
      marseilles_army:         [355,689],
      piedmont_army:           [434,692],
      tuscany_army:            [484,758],
      rome_army:               [515,793],
      naples_army:             [561,846],
      apulia_army:             [570,820],
      venice_army:             [478,708],
      kiel_army:               [460,486],
      berlin_army:             [530,475],
      prussia_army:            [609,480],
      ruhr_army:               [452,527],
      munich_army:             [478,564],
      silesia_army:            [578,536],
      livonia_army:            [698,440],
      finland_army:            [709,247],
      st_petersburg_army:      [892,273],
      warsaw_army:             [654,539],
      ukraine_army:            [760,586],
      moscow_army:             [842,447],
      sevastopol_army:         [860,608],
      armenia_army:            [1056,789],
      ankara_army:             [937,804],
      constantinople_army:     [800,850],
      smyrna_army:             [864,880],
      syria_army:              [1035,904]
    };

    var countryColor = {
        austria:[165,76,77],
        england:[96,101,169],
        france:[75,189,193],
        germany:[128,130,129],
        italy:[66,138,56],
        russia:[254,255,252],
        turkey:[218,172,24]
    };

    var countryOffset = {
        austria:[7,0],
        england:[4,5],
        france:[-2,7],
        germany:[-6,3],
        italy:[-6,-3],
        russia:[-2,-7],
        turkey:[4,-5]
    };

    var spawnPoint = {
        austria:[["budapest",1],["trieste",3],["vienna",1]],
        england:[["edinburgh",3],["liverpool",3],["london",3]],
        france:[["brest",3],["marseilles",3],["paris",1]],
        germany:[["berlin",3],["kiel",3],["munich",1]],
        russia:[["moscow",1],["sevastopol",3],["st_petersburg",3],["warsaw",1]],
        turkey:[["ankara",3],["constantinople",3],["smyrna",3]],
        italy:[["naples",3],["rome",3],["venice",3]]
    };

    var punits = map_data.punits;
    var mycountry = map_data.country;
    var phase = map_data.phase;
    var resOrd = map_data.resOrd;
    var spawnLookUp;
    mouseDrag = false;
    context = document.getElementById('canvas').getContext("2d");
    supportedUnits = new Object();
    orderCache = new Object();
    centerCount = 0;

    for(var i in owners){
        if(owners[i]==mycountry)
            ++centerCount;
    }

    if(phase == "order_phase"){
        for(var i in punits){
            var ut = (punits[i]=="army"?1:2);
            orderCache[i+ut] = {
                unit1:ut,
                loc1:i,
                act1:"h",
                target:"",
                tgCountry:mycountry
            };
        }
    }else if(phase == "retreat_phase"){
        for(var i in resOrd){
            var ord = resOrd[i];
            var u = ord.u1;
            var l = ord.l1;
            var ut = (u=="army"?1:2);
            if(ord.c1==mycountry){
                orderCache[l+ut] = {
                    unit1:ut,
                    loc1:l,
                    act1:"r",
                    target:""
                };
            }
        }
    }else{//build_phase
        spawnLookUp = new Object();
        for(var i=0; i < resOrd.length; i++){
            var c = resOrd[i];
            if(c.country == mycountry){
                buildNum = parseInt(c.count);
            }
        }
        if(buildNum>0){
            var spawn = spawnPoint[mycountry];
            disbandNum = 0;
            for(var i=0; i<spawn.length; i++){
                var myspawn = spawn[i]
                var prov = myspawn[0];
                spawnLookUp[prov] = myspawn[1];
                if(!units[prov+"__"+mycountry]){
                    if(owners[prov]==mycountry){
                        orderCache[prov] = {
                            loc1:prov,
                            act1:"b",
                            build:""
                        };
                    }
                }
            }
        }else if(buildNum<0){
            disbandNum = -buildNum;
            buildNum = "d";
            for(var i in punits){
                var ut = (punits[i]=="army"?1:2);
                orderCache[i+ut] = {
                    unit1:ut,
                    loc1:i,
                    act1:"d",
                    disband:false
                };
            }
        }else{
            disbandNum = 0;
            buildNum = 0;
        }
    }

    var refresh = function(){
        var oldCode = 0;
        var tiptext;
        unit_arr.length = 0;

        var mouseMove = function(e){
            var mX = e.pageX - canvas_pos.offsetLeft;
            var mY = e.pageY - canvas_pos.offsetTop;

            var code = getPixR(mX, mY);
            var provVal = provArea[code];
            if(code>75){
                var u = unit_arr[provVal];
                tiptext = (u[1]+"'s "+(u[0]==1?"Army":"Fleet")+" at "+(u[2].replace(/_/g, " "))).capitalize();
            }else if(code){
                tiptext = provVal.replace(/_/g, " ").capitalize();
                var ord = orderCache[provVal];
                if(phase=="build_phase" && ord){
                    if(ord.build)
                        tiptext = "Cancel Build";
                    else
                        tiptext = "Build Unit at "+tiptext;
                }
                mOX = mX;
                mOY = mY;
            }else{
                mX = mOX;
                mY = mOY;
            }

            if(code){
                if(mouseDrag){
                    if(code < 76){//overProv
                        var color;
                        if(provVal==selectedUnit[2]){//inUnitRegion
                            if(phase=="order_phase"){
                                tiptext = "Hold at " + tiptext;
                                color = "#0d0";
                            }else{
                                tiptext = "Undo Retreat";
                                color = "#d00";
                            }
                            drawBGBuf();
                            drawFG();
                            drawArrow(mA, [mX, mY],color);
                        }else if(oldCode != code){//onChangeRegion
                            if(phase=="order_phase"){
                                tiptext = "Move to " + tiptext;
                                color = "#0d0";
                            }else{
                                tiptext = "Retreat to " + tiptext;
                                color = "#d00";
                            }
                            drawBGBuf();
                            colorLayerData = context.getImageData(0, 0, canvasWidth, canvasHeight);
                            highlight(code, mapBGBuf);
                            drawArrow(mA, coords[provArea[getPixR(mOX, mOY)]],color);
                        }else{
                            if(phase=="order_phase")
                                tiptext = "Move to " + tiptext;
                            else
                                tiptext = "Retreat to " + tiptext;
                        }
                    }else{//overUnit
                        if(isMyUnit){//inUnitRegion
                            var tgUnit = unit_arr[provVal];
                            if(tgUnit[2]==selectedUnit[2]){
                                tiptext = "Hold";
                            }else{
                                if(selectedUnit[0]==1 || tgUnit[0]==2){
                                //selected unit is army or target unit is fleet
                                    tiptext = "Support " + tiptext;
                                }else{//selected unit is fleet and target unit is army
                                    tiptext = "Assist " + tiptext;
                                }
                            }
                            if(oldCode != code){//onChangeRegion
                                var u = unit_arr[provVal];
                                var coord = unitPosCrt(coordinates[u[2]+(u[0]==1?"_army":"_fleet")], u[0], u[1]);
                                drawBG();
                                drawHalo(coord,"#d00",10);
                                drawFG();
                            }
                        }else{
                            tiptext = "Cancel Move";
                        }
                    }
                }else{
                    if(code < 76){//overProv
                        if(oldCode != code){//onChangeRegion
                            drawBG();
                            highlight(code, colorLayerData);
                        }
                    }else{//overUnit
                        var u = unit_arr[provVal];
                        var coord = unitPosCrt(coordinates[u[2]+(u[0]==1?"_army":"_fleet")], u[0], u[1]);
                        if(oldCode != code){//onChangeRegion
                            drawBG();
                            drawHalo(coord,"#0d0",10);
                            drawFG();
                        }
                    }
                }
                oldCode = code;
            }

            tooltip.show('<h3><font color="white">'+tiptext+'</font></h3>');
        };

        var mouseDown = function(e){
             var mX = e.pageX - canvas_pos.offsetLeft;
             var mY = e.pageY - canvas_pos.offsetTop;
             var code = getPixR(mX, mY);
             if(phase == "build_phase"){
                 var prov = provArea[code];
                 var ord = orderCache[prov];
                 tooltip.disMenu();
                 tooltip.hide();
                 if(code<76){
                     if(owners[prov]==mycountry && ord){
                         if(ord.build){
                             ++buildNum;
                             ord.build = "";
                             drawBG();
                             drawFG();
                             order_generate();
                         }else{
                             if(buildNum>0){
                                 if(spawnLookUp[prov] == 3){
                                     var menuCont =
                                     '<a href="javascript:void(0);" class="btn primary" onclick="menuBuild(\'army\');">Army</a>&nbsp<br>'+
                                     '<a href="javascript:void(0);" class="btn primary" onclick="menuBuild(\'fleet\');">Fleet</a>&nbsp';
                                     tooltip.enMenu(menuCont);
                                     selectedUnit = prov;
                                 }else{
                                     --buildNum;
                                     ord.build = "army";
                                     drawBG();
                                     drawFG();
                                     order_generate();
                                 }
                             }
                         }
                     }
                 }else{//unit

                     var tempUnit = unit_arr[provArea[code]];
                     var disUnit = tempUnit[2]+tempUnit[0];
                     var ord = orderCache[disUnit];
                     if(buildNum == "d"){
                         if(disbandNum>0 || ord.disband){
                             if(ord){
                                 if(ord.disband){
                                     ord.disband = false;
                                     ++disbandNum;
                                 }else{
                                     ord.disband = true;
                                     --disbandNum;
                                 }
                                 drawBG();
                                 drawFG();
                                 order_generate();
                             }
                         }
                     }
                 }
             }else{//retreat or order
                 var tempUnit;
                 var run = function(){
                     selectedUnit = tempUnit;
                     mouseDrag = true;
                     drawBG();
                     showNeighbor(tempUnit[2], tempUnit[0]);
                     getBGBuf();
                     drawFG();
                     mA[0] = mX;
                     mA[1] = mY;
                 }

                 if(code>75){
                     tempUnit = unit_arr[provArea[code]];
                     var supportee = tempUnit[2]+tempUnit[0];

                     if(tempUnit[1]==mycountry && orderCache[supportee]){
                         isMyUnit = true;
                         run();
                     }else if(supportedUnits[supportee]){
                         isMyUnit = false;
                         run();
                     }
                 }
                 tooltip.disMenu();
             }
        };

        var mouseUp = function(e){
            if(mouseDrag){
                var mX = e.pageX - canvas_pos.offsetLeft;
                var mY = e.pageY - canvas_pos.offsetTop;
                var code = getPixR(mX, mY);


                var ord = orderCache[selectedUnit[2]+selectedUnit[0]];
                if(code>75){//over unit
                    if(phase == "order_phase"){
                        if(isMyUnit){//my unit
                            cutSupport(ord.target);
                            var tgUnit = unit_arr[provArea[code]];
                            ord.target = tgUnit[2]+tgUnit[0];
                            ord.tgCountry = tgUnit[1];
                            if(tgUnit[2]==selectedUnit[2]){//hold
                                ord.act1 = "h";
                                order_generate();
                            }else{
                                if(selectedUnit[0]==1 || tgUnit[0]==2){//support
                                //selected unit is army or target unit is fleet
                                    ord.act1 = "s";
                                    addSupport(ord.target, "h", tgUnit[1]);
                                    order_generate();
                                }else{//Convoy or Support
                                    var menuCont =
                                    '<a href="javascript:void(0);" class="btn primary" onclick="menuInput(false,\''+tgUnit[1]+'\');">Convoy</a>&nbsp<br>'+
                                    '<a href="javascript:void(0);" class="btn primary" onclick="menuInput(true,\''+tgUnit[1]+'\');">Support</a>&nbsp';
                                    ord.act1 = "h";
                                    tooltip.enMenu(menuCont);
                                }
                            }
                        }else{//other's unit cancel
                            var ord = supportedUnits[selectedUnit[2]+selectedUnit[0]];
                            ord.act = "h";
                            ord.to = "";
                        }
                    }else{
                        ord.target = "";
                    }
                }else{//over prov for move
                    if(!code)
                        code = oldCode;
                    if(isMyUnit){//my unit
                        var prov = provArea[code];
                        if(phase=="order_phase"){
                            cutSupport(ord.target);
                            if(prov == selectedUnit[2]){
                                ord.act1 = "h";
                            }else{
                                ord.act1 = "m";
                                ord.target = prov;
                            }
                        }else{//retreat
                            if(ord){
                                if(prov == selectedUnit[2]){
                                    ord.target = "";
                                }else{
                                    ord.target = prov;
                                }
                            }
                        }
                    }else{//supported unit from other country
                        var ord = supportedUnits[selectedUnit[2]+selectedUnit[0]];
                        var prov = provArea[code];
                        if(prov == selectedUnit[2]){//hold
                            ord.act = "h";
                            ord.to = "";
                        }else{//move
                            ord.act = "m";
                            ord.to = prov;
                        }
                    }
                }
                order_generate();
                drawBG();
                drawFG();
            }
            mouseDrag = false;
        }

        var retreatMove = function(){

        };
        var retreatDown = function(){

        };
        var retreatUp = function(){

        };

        var buildMove = function(){

        };
        var buildDown = function(){

        };
        var buildUp = function(){

        };

        $('#canvas').mousemove(mouseMove);
        $('#canvas').mousedown(mouseDown);
        $('#canvas').mouseup(mouseUp);

        $('#canvas').mouseenter(function(){
            document.body.onmousedown=function(){return false;};
            document.body.oncontextmenu=function(){return false;};
            window.onmouseup = mouseUp;
            document.onchange=function(){
                window.onmouseup=null;
            }
        });

        $('#canvas').mouseleave(function(e){
            tooltip.hide();
            document.body.onmousedown=null;
            document.body.oncontextmenu=null;
        });

        context.putImageData(provLookUp, 0, 0);
        tooltip.load(function(){drawBG();drawFG();});

        var n = 76;
        var n2 = 0;
        for(var unit in units) {
            var prov_country=unit.split("__");
            var province = prov_country[0];
            var country = prov_country[1];
            if (units[unit] == "army") {
                var key = province + '_army';
                var coord = coordinates[key];
                var offset = countryOffset[country];
                armyMask(coord[0]+offset[0],coord[1]+offset[1], n);
                unit_arr.push([1, country, province]);
            } else if (units[unit] == "fleet") {
                var key = province + '_fleet';
                var coord = coordinates[key];
                var offset = countryOffset[country];
                fleetMask(coord[0]+offset[0],coord[1]+offset[1], n);
                unit_arr.push([2, country, province]);
            }
            provArea.push(n2);
            ++n2;
            ++n;
        }
        puLookUp = context.getImageData(0, 0, canvasWidth, canvasHeight);

        context.drawImage(mapBGImage, 0, 0, canvasWidth, canvasHeight);
        colorLayerData = context.getImageData(0, 0, canvasWidth, canvasHeight);
        for(var i in owners){
            var color = countryColor[owners[i]];
            newColorR= color[0];
            newColorG= color[1];
            newColorB= color[2];
            paintAt(coords[i]);
        }
        coloredBG = context.getImageData(0, 0, canvasWidth, canvasHeight);

        drawBG();
        drawFG();
    }//refresh_end

    var mapBGBuf;
    var getBGBuf = function(){
        mapBGBuf = context.getImageData(0, 0, canvasWidth, canvasHeight);
    }

    var clearCanvas = function(){
        context.globalAlpha = 0;
        context.fillRect(0, 0, canvasWidth, canvasHeight);
        context.globalAlpha = 1;
    }

    var highlight = function(code, mapdata){
        var coord = coords[provArea[code]];
        colorPicker(mapdata, coord[0], coord[1]);
        newColorR-= 30;
        newColorG-= 30;
        newColorB-= 30;
        paintAt(coord);
        drawFG();
    }

    var drawBGBuf = function(){
        context.putImageData(mapBGBuf, 0, 0);
    }

    var drawBG = function(){
        context.putImageData(coloredBG, 0, 0);
        colorLayerData = context.getImageData(0, 0, canvasWidth, canvasHeight);
    }

    var drawFG = function(){
        for(var province in owners){
            var country = owners[province];
            var key = province + '_center';
            var x = coordinates[key][0];
            var y = coordinates[key][1];
            context.drawImage(document.getElementById(country+'_flag'), x, y);
        }
        context.drawImage(mapFGImage, 0, 0, canvasWidth, canvasHeight);

        vizOrders();
        for(var unit in units){
            var prov_country=unit.split("__");
            var province = prov_country[0];
            var country = prov_country[1];
            var offset = countryOffset[country];
            var suffix = "_" + units[unit];
            var key = province + suffix;
            var x = coordinates[key][0];
            var y = coordinates[key][1];
            context.drawImage(document.getElementById(country+suffix), x+offset[0], y+offset[1]);
        }
        context.fillStyle = "white";
        context.font = "14pt Arial";
        context.fillText("Phase: "+phase, 20, 40);
        context.fillText("Supply Centers: "+centerCount, 20, 60);
    }

    var vizOrders = function(){
        for(var i in supportedUnits){
            var ord = supportedUnits[i];
            var last = i.length-1;
            var prov = i.substring(0, last);
            var tgUnit = parseInt(i[last]);
            var p1 = coordinates[prov+"_"+(tgUnit==1?"army":"fleet")];
            var p1c = unitPosCrt(p1, tgUnit, ord.country);
            if(ord.to){
                drawArrow(p1c, coords[ord.to],"#0c0");
            }
        }

        for(var i in orderCache){
            var ord = orderCache[i];
            var act1 = ord.act1;
            var prov1 = ord.loc1;
            var unit1 = ord.unit1;
            if(act1 == "b"){
                var build = ord.build;
                var coord = coords[prov1];
                if(buildNum>0||build)
                    drawHalo(coord,"#0d0",14);
                if(build){
                    if(build=="army"){
                        context.drawImage(document.getElementById(mycountry+"_army"), coord[0]-12, coord[1]-10);
                    }else{
                        context.drawImage(document.getElementById(mycountry+"_fleet"), coord[0]-17, coord[1]-7);
                    }
                }
                continue;
            }else if(act1 == "d"){
                var disband = ord.disband;
                var coord = coordinates[prov1+"_"+(unit1==1?"army":"fleet")];
                var crtCoord = unitPosCrt(coord, unit1, mycountry);
                if(disband){
                    drawCross(crtCoord,"#d00");
                }else{
                    if(disbandNum>0)
                        drawHalo(crtCoord,"#d00",14);
                }
                continue;
            }
            var p1 = coordinates[prov1+"_"+(unit1==1?"army":"fleet")];
            var p1c = unitPosCrt(p1, unit1, mycountry);
            var tg = ord.target;
            var len = tg.length;
            var last = len-1;
            var prov = tg.substring(0, last);
            var tgUnit = parseInt(tg[last]);
            var p2 = coordinates[prov+"_"+(tgUnit==1?"army":"fleet")];

            switch(act1){
                case "m":
                    drawArrow(p1c, coords[tg],"#0c0");
                break;
                case "r":
                    if(tg){
                        drawArrow(p1c, coords[ord.target],"#c00");
                    }else{
                        var coord = coordinates[prov1+"_"+(unit1==1?"army":"fleet")];
                        var crtCoord = unitPosCrt(coord, unit1, mycountry);
                        drawCross(crtCoord,"#d00");
                    }
                break;
                case "h":
                    drawHalo(p1c,"#0d0",18);
                break;
                case "s":
                    var p2c = unitPosCrt(p2, tgUnit, ord.tgCountry);
                    var tgOrd = orderCache[tg];
                    if(tgOrd){//tg is my unit
                        if(tgOrd.act1=="s"||tgOrd.act1=="c"){
                            drawCross(p1c,"#dd0");
                        }
                    }else{//tg is unit of other countries
                        if(supportedUnits[tg].act=="h"){
                            drawHalo(p2c,"#dd0",18);
                        }
                    }
                    drawArrow(p1c, p2c,"#cc0");
                break;
                case "c":
                    var p2c = unitPosCrt(p2, tgUnit, ord.tgCountry);
                    var tgOrd = orderCache[tg];
                    if(tgOrd){//my unit
                        if(tgOrd.act1!="m"){
                            drawCross(p1c,"#00d");
                        }
                    }else{//other's unit
                        if(supportedUnits[tg].act=="h"){
                            drawCross(p1c,"#00d");
                            drawHalo(p2c,"#00d",18);
                        }
                    }
                    drawArrow(p1c, p2c,"#00c");
                break;
            }
        }
    }

    var colorPicker = function(imgdata, xPos, yPos){
        var pixelPos = (xPos+yPos*canvasWidth)*4
        newColorR = imgdata.data[pixelPos];
        newColorG = imgdata.data[pixelPos+1];
        newColorB = imgdata.data[pixelPos+2];
    }

    var showNeighbor = function(prov, unit){
        var adj = prov_adj[prov];
        var color = countryColor[mycountry];
        newColorR= 75;
        newColorG= 220;
        newColorB= 75;
        for(var i=0; i<adj.length; i++){
            if(unit&adj[i][0]){
                paintAt(coords[adj[i][1]]);
            }
        }
    }

    var unitPosCrt = function(pos, unit, country){
        var offset = countryOffset[country];
        if(unit==1){
            return [pos[0]+offset[0]+12, pos[1]+offset[1]+10];
        }
        return [pos[0]+offset[0]+17, pos[1]+offset[1]+7];
    }

    if(mapLoaded){
        refresh();
    }else{
        var mapOnload = function(){
            if(++loadedMap>=3){
                String.prototype.capitalize = function(){
                    return this.replace( /(^|\s)([a-z])/g, function(m,p1,p2){return p1+p2.toUpperCase();});
                };

                context.drawImage(mapDataImage, 0, 0, canvasWidth, canvasHeight);
                colorLayerData = context.getImageData(0, 0, canvasWidth, canvasHeight);
                provLookUp = context.getImageData(0, 0, canvasWidth, canvasHeight);

                refresh();
                mapLoaded = true;
                order_generate();
            }
        }

        mapDataImage.onload = mapOnload;
        mapBGImage.onload = mapOnload;
        mapFGImage.onload = mapOnload;
        mapDataImage.src = "image/lookup.png";
        mapBGImage.src = "image/bgmap.gif";
        mapFGImage.src = "image/fgmap.gif";
    }
}

function stringfy(js){
    var acc = "{";
    for(var i in js){
        var data=js[i];
        acc += i+':['+data[0]+','+data[1]+'],';
    }
    acc+="}";
    return acc;
}

function addSupport(supportee, actType, country){
    var supportedUnit = supportedUnits[supportee];
    if(supportedUnit){
        supportedUnit.act = actType;
        supportedUnit.to = "";
        supportedUnit.cnt++;
    }else{
        supportedUnits[supportee] = {act:"h", to:"", cnt:1, country:country};
    }
}

function cutSupport(supportee){
    var supportedUnit = supportedUnits[supportee];
    if(supportedUnit){
        supportedUnit.cnt--;
        if(!supportedUnit.cnt){
            delete supportedUnits[supportee];
        }
    }
}

function menuInput(support, country){
    var supporter = selectedUnit[2]+selectedUnit[0];
    var ord = orderCache[supporter];

    if(support){
        ord.act1 = "s";
        addSupport(ord.target, "h", country);
    }else{//convoy
        ord.act1 = "c";
        addSupport(ord.target, "m", country);
    }

    tooltip.disMenu();
    tooltip.hide();
    order_generate();
    //document.body.style.cursor = "default";
}

function menuBuild(unit){
    var ord = orderCache[selectedUnit];
    --buildNum;
    ord.build = unit;

    tooltip.disMenu();
    tooltip.hide();
    order_generate();
}

function supportableArea(prov, unit){
    var dict = new Object;
    var ring1 = prov_adj[prov];
    for(var i=0; i<ring1.length; i++){
        var prov2 = ring1[i][1];
        var ring2 = prov_adj[prov2];
        dict[prov2]=true;
        for(var j=0; j<ring2.length; j++){
            dict[ring2[j][1]]=true;
        }
    }
    delete dict[prov];

    if(newColorR>128)
        newColorR = 50;
    else
        newColorR = 255;
    for(var i in dict){
        paintAt(coords[i]);
    }
}

function armyMask(x, y, id){
    context.fillStyle = "rgb("+id+",0,0);";
    context.fillRect(x, y, 25, 20);
}

function fleetMask(x, y, id){
    context.fillStyle = "rgb("+id+",0,0);";
    context.fillRect(x, y, 35, 15);
}

/**
 * Paint based on mouse click location
 */
function paintAt(pos)
{
    flood(pos[0], pos[1]);
}

/**********************************************************/
function flood(startX, startY)
{
    var pixelPos = (startY*canvasWidth + startX)*4;

    var r = colorLayerData.data[pixelPos + 0];
    var g = colorLayerData.data[pixelPos + 1];
    var b = colorLayerData.data[pixelPos + 2];
    var a = colorLayerData.data[pixelPos + 3];
    //console.log("clicked color:   " + r +","+ g +","+ b +","+ a);

    clickedColorR = r;
    clickedColorG = g;
    clickedColorB = b;

    //console.log("new color:   " + newColorR +","+ newColorG +","+ newColorB);

    if(clickedColorR == newColorR && clickedColorG == newColorG && clickedColorB == newColorB)
    {
        //console.log("Return because trying to fill with the same color");
        return;
    }

    //console.log("PUSH: " + (startX - drawingAreaX - 2) + "," + (startY - drawingAreaY - 2));
    pixelStack = [[startX, startY]];

    floodFill();
}

function floodFill()
{
    var newPos, x, y, pixelPos, reachLeft, reachRight;
    var drawingBoundLeft = drawingAreaX;
    var drawingBoundTop = drawingAreaY;
    var drawingBoundRight = drawingAreaX + drawingAreaWidth - 1;
    var drawingBoundBottom = drawingAreaY + drawingAreaHeight - 1;

    while(pixelStack.length){
        newPos = pixelStack.pop();
        x = newPos[0];
        y = newPos[1];

        //console.log("POP: " + (x - drawingAreaX - 2) + "," + (y - drawingAreaY - 2));

        pixelPos = (y*canvasWidth + x) * 4;
        // Go up as long as the color matches and are inside the canvas
        while(y-- >= drawingBoundTop && matchClickedColor(pixelPos))
        {
            //console.log("UP: " + (x - drawingAreaX - 2) + "," + (y - drawingAreaY - 2));
            pixelPos -= canvasWidth * 4;
        }
        pixelPos += canvasWidth * 4;
        ++y;
        reachLeft = false;
        reachRight = false;
        // Go down as long as the color matches and in inside the canvas
        while(y++ < drawingBoundBottom && matchClickedColor(pixelPos))
        {
            colorPixel(pixelPos);
            //console.log("COLOR: " + (x - drawingAreaX - 2) + "," + (y - drawingAreaY - 2));

            if(x > drawingBoundLeft)
            {
                if(matchClickedColor(pixelPos - 4)){
                    if(!reachLeft){
                        pixelStack.push([x - 1, y]);
                        //console.log("PUSH: " + ((x-1) - drawingAreaX - 2) + "," + (y - drawingAreaY - 2));
                        reachLeft = true;
                    }
                }else if(reachLeft){
                    reachLeft = false;
                }
            }
            if(x < drawingBoundRight)
            {
                if(matchClickedColor(pixelPos + 4)){
                    if(!reachRight){
                        pixelStack.push([x + 1, y]);
                        //console.log("PUSH: " + ((x+1) - drawingAreaX - 2) + "," + (y - drawingAreaY - 2));
                        reachRight = true;
                    }
                }else if(reachRight){
                    reachRight = false;
                }
            }

            pixelPos += canvasWidth * 4;
        }
    }
    context.putImageData(colorLayerData, 0, 0);
}

function matchClickedColor(pixelPos)
{
    var r = colorLayerData.data[pixelPos];
    var g = colorLayerData.data[pixelPos+1];
    var b = colorLayerData.data[pixelPos+2];
    var a = colorLayerData.data[pixelPos+3];

    // If current pixel is black then it is an outline
    if(r + g + b == 0){ return false; }

    r = colorLayerData.data[pixelPos];
    g = colorLayerData.data[pixelPos+1];
    b = colorLayerData.data[pixelPos+2];

    // If the current pixel matches the clicked color
    if(r == clickedColorR && g == clickedColorG && b == clickedColorB) return true;

    // If current pixel matches the new color
    //if(r == newColorR && g == newColorG && b == newColorB) return false;

    return false;
}

function colorPixel(pixelPos)
{
    colorLayerData.data[pixelPos] = newColorR;
    colorLayerData.data[pixelPos+1] = newColorG;
    colorLayerData.data[pixelPos+2] = newColorB;
    colorLayerData.data[pixelPos+3] = 255;
}

function getPixR(xPos, yPos){
    return puLookUp.data[(xPos+yPos*canvasWidth)*4];
}

function drawArrowhead(locx, locy, angle, sizex, sizey) {
    var hx = sizex / 2;
    var hy = sizey / 2;
    context.translate((locx ), (locy));
    context.rotate(angle);
    context.translate(-hx,-hy);

    context.beginPath();
    context.moveTo(0,0);
    context.lineTo(0,1*sizey);
    context.lineTo(1*sizex,1*hy);
    context.closePath();
    context.fill();
    context.setTransform(1, 0, 0, 1, 0, 0);
}


// returns radians
function findAngle(sx, sy, ex, ey) {
    // make sx and sy at the zero point
    if(ex < sx)
        return Math.atan((ey - sy) / (ex - sx)) + Math.PI;
    return Math.atan((ey - sy) / (ex - sx));
}

function drawArrow(start, end, color){
    var sx = start[0];
    var sy = start[1];
    var ex = end[0];
    var ey = end[1];
    var mx;
    var my;

    if(Math.abs(ex-sx) > Math.abs(ey-sy)){
        mx = sx;
        my = ey;
    }else{
        mx = ex;
        my = sy;
    }

    context.beginPath();
    context.fillStyle = color;
    context.strokeStyle = color; // red
    context.lineWidth   = 4;
    context.moveTo(sx, sy);
    context.quadraticCurveTo(mx, my, ex, ey);
    context.stroke();


    var ang = findAngle(mx, my, ex, ey);
    drawArrowhead(ex, ey, ang, 19, 14);

}

function drawHalo(pos, color, radius){
    context.beginPath();
    context.arc(pos[0], pos[1], radius, 0, 2 * Math.PI, false);
    context.lineWidth = 4;
    context.strokeStyle = color;
    context.stroke();
}

function drawCross(pos, color){
    context.beginPath();
    context.lineWidth = 4;
    context.strokeStyle = color;
    context.moveTo(pos[0]-20, pos[1]-20);
    context.lineTo(pos[0]+20, pos[1]+20);
    context.moveTo(pos[0]+20, pos[1]-20);
    context.lineTo(pos[0]-20, pos[1]+20);
    context.stroke();
}

function order_generate(){
    var text = "";
    for(var i in orderCache){
        var ord = orderCache[i];
        var unit1 = (ord.unit1==1?"army ":"fleet ");
        switch(ord.act1){
            case "r":
            case "m":
                text+=unit1+ord.loc1+" -> "+ord.target+"\n";
            break;
            case "h":
                text+=unit1+ord.loc1+" hold\n";
            break;
            case "s":
                var tg = ord.target;
                var last = tg.length-1;
                var prov = tg.substring(0, last);
                var unit = (tg[last]=="1"?"army ":"fleet ");
                var myOrd = orderCache[tg];
                text+=unit1+ord.loc1+" support "+unit+prov;
                if(myOrd){
                    if(myOrd.act1=="h"){
                        text+=" hold\n";
                    }else{
                        text+=" -> "+myOrd.target+"\n";
                    }
                }else{
                    var su = supportedUnits[tg];
                    if(su.act=="h"){
                        text+=" hold\n";
                    }else{
                        text+=" -> "+su.to+"\n";
                    }
                }
            break;
            case "c":
                var tg = ord.target;
                var last = tg.length-1;
                var prov = tg.substring(0, last);
                var unit = (tg[last]=="1"?"army ":"fleet ");
                var cu = supportedUnits[tg];
                var myOrd = orderCache[tg];
                text+=unit1+ord.loc1+" convoy "+unit+prov+" -> ";
                if(myOrd){
                    text+=myOrd.target+"\n";
                }else{
                    text+=cu.to+"\n";
                }
            break;
            case "b":
                if(ord.build)
                    text+="build "+ord.build+" "+ord.loc1+"\n";
            break;
            case "d":
                if(ord.disband)
                    text+="disband "+(ord.unit1==1?"army ":"fleet ")+ord.loc1+"\n";
            break;
        }
    }
    $('#game_order').val(text);
}

function pix(pos){
    newColorR = 255;
    newColorG = 255;
    newColorB = 255;
    colorPixel((pos[0]+pos[1]*canvasWidth)*4);
}
