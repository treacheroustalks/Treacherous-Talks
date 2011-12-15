function draw(units, owners) {
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
      york_fleet:              [338,441],
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
      york_army:               [337,441],
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

  var ctx = document.getElementById('canvas').getContext('2d');
  var map = new Image();
  map.src = '/image/Components/Map.png';
  map.onload = function() {
      //Draw Map
      ctx.drawImage(map,0,0);

      for(var province in owners) {
          var country = owners[province];
          var key = province + '_center';
          var x = coordinates[key][0];
          var y = coordinates[key][1];
          print(document.getElementById(country+'_flag'));
          ctx.drawImage(document.getElementById(country+'_flag'), x, y);
      }

      for(var unit in units) {
          var prov_country=unit.split("__");
          var province = prov_country[0];
          var country = prov_country[1];
          var randomNumber = Math.random();
          var shift = ((randomNumber == 0 || randomNumber == 1) ? 3 : randomNumber) * (10 - 3) + 3;
          if (units[unit] == "army") {
              var key = province + '_army';
              var x = coordinates[key][0];
              var y = coordinates[key][1];
              ctx.drawImage(document.getElementById(country+'_army'), x+shift, y);
          }
          else if (units[unit] == "fleet") {
              var key = province + '_fleet';
              var x = coordinates[key][0];
              var y = coordinates[key][1];
              ctx.drawImage(document.getElementById(country+'_fleet'), x+shift, y);
          }
      }
  };
}

function primeOnChangeEvents() {
    var selects = document.getElementsByTagName('select');
    for(i in selects) {
        if(selects[i].options) selects[i].onchange = selectChange;
    }
}

function enable(ID) {
    $('#'+ID).show();
    document.getElementById(ID).disabled = false;
}

function disable(ID) {
    $('#'+ID).hide();
    document.getElementById(ID).disabled = true;
}

function selectChange() {
    var vals = this.options[this.selectedIndex].value.split(" ");
    for(var i in vals) {
        var val = vals[i].split(":");
        if(val[0] == 'e') enable(val[1]);
        else if(val[0] == 'd') disable(val[1]);
        else if(val[0] == 'c'){
            enable(val[1]);
            $('#'+'m'+val[1]).prop('selected', true);
        }else if(val[0] == 's'){
            enable(val[1]);
            $('#'+'h'+val[1]).prop('selected', true);
        }
    }
    order_generate();
}

function order_generate() {
    var selects = $('.order_select');
    var attr, orders = "";
    print(selects);
    for(var i=0; i<selects.length; i++) {
        attr = selects[i].options[selects[i].selectedIndex].innerHTML;   //selects[i].getAttribute('name');
        orders += (attr === null || attr === "") ? "" :
            selects[i].disabled ? "" :
                attr+" ";
        if((i+1)%7 === 0) { orders += "\n"; }
    }
    $('#game_order').val(orders);
}

var rownum;
function add_order_row(unit, prov) {
    if(rownum < 17) {
        ++rownum;
        var el = document.getElementById('order_gen');
        var divi = document.createElement('div');

        /*var sel = document.createElement('select');
        sel.style.width = "75px";
        sel.id = "unit"+rownum;

        var opt = createElement('option');
        opt.innerHTML = &nbsp;
        sel.appendChild(opt);*/
        var prov_options = "<option selected=\"selected\"></option>\n"+
                "<option>Adriatic_Sea</option>\n"+
                "<option>Aegean_Sea</option>\n"+
                "<option>Albania</option>\n"+
                "<option>Ankara</option>\n"+
                "<option>Apulia</option>\n"+
                "<option>Armenia</option>\n"+
                "<option>Baltic_Sea</option>\n"+
                "<option>Barents_Sea</option>\n"+
                "<option>Belgium</option>\n"+
                "<option>Berlin</option>\n"+
                "<option>Black_Sea</option>\n"+
                "<option>Bohemia</option>\n"+
                "<option>Brest</option>\n"+
                "<option>Budapest</option>\n"+
                "<option>Bulgaria</option>\n"+
                //"<option>Bulgaria (North coast)</option>\n"+
                //"<option>Bulgaria (South coast)</option>\n"+
                "<option>Burgundy</option>\n"+
                "<option>Clyde</option>\n"+
                "<option>Constantinople</option>\n"+
                "<option>Denmark</option>\n"+
                "<option>Eastern_Mediterranean</option>\n"+
                "<option>Edinburgh</option>\n"+
                "<option>English Channel</option>\n"+
                "<option>Finland</option>\n"+
                "<option>Galicia</option>\n"+
                "<option>Gascony</option>\n"+
                "<option>Greece</option>\n"+
                "<option>Gulf_of_Lyon</option>\n"+
                "<option>Gulf_of_Bothnia</option>\n"+
                "<option>Helgoland_Bight</option>\n"+
                "<option>Holland</option>\n"+
                "<option>Ionian_Sea</option>\n"+
                "<option>Irish_Sea</option>\n"+
                "<option>Kiel</option>\n"+
                "<option>Liverpool</option>\n"+
                "<option>Livonia</option>\n"+
                "<option>London</option>\n"+
                "<option>Marseilles</option>\n"+
                "<option>Mid_Atlantic_Ocean</option>\n"+
                "<option>Moscow</option>\n"+
                "<option>Munich</option>\n"+
                "<option>Naples</option>\n"+
                "<option>North_Atlantic_Ocean</option>\n"+
                "<option>North_Africa</option>\n"+
                "<option>North_Sea</option>\n"+
                "<option>Norway</option>\n"+
                "<option>Norwegian Sea</option>\n"+
                "<option>Paris</option>\n"+
                "<option>Picardy</option>\n"+
                "<option>Piedmont</option>\n"+
                "<option>Portugal</option>\n"+
                "<option>Prussia</option>\n"+
                "<option>Rome</option>\n"+
                "<option>Ruhr</option>\n"+
                "<option>Rumania</option>\n"+
                "<option>Serbia</option>\n"+
                "<option>Sevastopol</option>\n"+
                "<option>Silesia</option>\n"+
                "<option>Skagerrak</option>\n"+
                "<option>Smyrna</option>\n"+
                "<option>Spain</option>\n"+
                //"<option>Spain (North coast)</option>\n"+
                //"<option>Spain (South coast)</option>\n"+
                "<option>St_Petersburg</option>\n"+
                //"<option>St_Petersburg (North coast)</option>\n"+
                //"<option>St_Petersburg (South coast)</option>\n"+
                "<option>Sweden</option>\n"+
                "<option>Syria</option>\n"+
                "<option>Trieste</option>\n"+
                "<option>Tunis</option>\n"+
                "<option>Tuscany</option>\n"+
                "<option>Tyrolia</option>\n"+
                "<option>Tyrrhenian_Sea</option>\n"+
                "<option>Ukraine</option>\n"+
                "<option>Venice</option>\n"+
                "<option>Vienna</option>\n"+
                "<option>Wales</option>\n"+
                "<option>Warsaw</option>\n"+
                "<option>Western_Mediterranean</option>\n"+
                "<option>Yorkshire</option>\n";
        var new_row =
            //"\n<br class=\"spacer\" />\n"+
            "<select class='order_select' id=\"unit"+rownum+"\" style=\"width:55px\">\n"+
                "<option selected>"+unit+"</option>\n"+
            "</select>\n\n"+

            "<select class='order_select' id=\"region"+rownum+"\">\n"+
                "<option selected value=\"e:order"+rownum+"\">"+prov+"</option>\n"+
            "</select>\n\n"+

            "<select class='order_select' id=\"order"+rownum+"\" style='width:70px'>\n"+
                "<option value=\"d:unitb"+rownum+" d:order2"+rownum+" e:target"+rownum+" d:target2"+rownum+"\">Move</option>\n"+
                "<option value=\"e:unitb"+rownum+" s:order2"+rownum+" e:target"+rownum+" d:target2"+rownum+"\">Support</option>\n"+
                "<option value=\"e:unitb"+rownum+" c:order2"+rownum+" e:target"+rownum+" e:target2"+rownum+"\">Convoy</option>\n"+
                "<option selected value=\"d:unitb"+rownum+" d:order2"+rownum+" d:target"+rownum+" d:target2"+rownum+"\">Hold</option>\n"+
            "</select>\n\n"+

            "<select class='order_select' id=\"unitb"+rownum+"\" disabled style=\"display:none;width:55px\">\n"+
                "<option selected>army</option>\n"+
                "<option>fleet</option>\n"+
            "</select>\n\n"+

            "<select class='order_select' id=\"target"+rownum+"\" disabled style='display:none'>\n"+
                prov_options+
            "</select>\n\n"+

            "<select class='order_select' id=\"order2"+rownum+"\" disabled style='display:none;width:58px'>\n"+
                "<option id=\"morder2"+rownum+"\" value=\"e:target2"+rownum+"\">Move</option>\n"+
                "<option id=\"horder2"+rownum+"\"selected value=\"d:target2"+rownum+"\">Hold</option>\n"+
            "</select>\n\n"+

            "<select class='order_select' id=\"target2"+rownum+"\" disabled style='display:none'>\n"+
                prov_options+
            "</select>\n\n";

        divi.innerHTML = new_row;
        el.appendChild(divi);
        el.lastChild.setAttribute('id', 'row'+rownum);
        primeOnChangeEvents();
    } else {
        alert("You are at max unit control (17).\nIf you had more than that, you'd have won already!");
    }
}
