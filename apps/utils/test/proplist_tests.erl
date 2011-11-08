-module(proplist_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").

update_rec_by_proplist_test_() ->
    [
       ?_assertEqual(
          #user{nick="lin", name="agner"},
          proplist:update_record(
              #user{nick="lin"},
              [{#user.password, field_missing}, {#user.name, "agner"}]
       )
     )].
