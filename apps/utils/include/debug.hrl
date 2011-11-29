-ifndef(DEBUG_TT).
-define(DEBUG_TT, true).
-define(DEBUG_ENABLE, true).
-ifdef(DEBUG_ENABLE).
-define(DEBUG(Format),
  io:format("~s.~w: DEBUG: " ++ Format, [ ?MODULE, ?LINE])).
-define(DEBUG(Format, Args),
  io:format("~s.~w: DEBUG: " ++ Format, [ ?MODULE, ?LINE | Args])).
-define(DEBUG(IoDevice, Format, Args),
  io:format(IoDevice, "~s.~w: DEBUG: " ++ Format, [ ?MODULE, ?LINE | Args])).
-else.
-define(DEBUG(Format), true).
-define(DEBUG(Format, Args), true).
-define(DEBUG(IoDevice, Format, Args), true).
-endif.
-endif.
