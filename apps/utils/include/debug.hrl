-ifndef(DEBUG_TT).
-define(DEBUG_TT, true).
%-define(DEBUG_ENABLE, true).
-ifdef(DEBUG_ENABLE).
-define(DEBUG(Format, Args),
  io:format("~s.~w: DEBUG: " ++ Format, [ ?MODULE, ?LINE | Args])).
-else.
-define(DEBUG(Format, Args), true).
-endif.
-endif.