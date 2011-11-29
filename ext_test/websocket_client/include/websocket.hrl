%% Corresponds to the frame sections as in
%% http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08#section-4
%% plus 'data' and 'ws_state'
-record(ws_frame_info, {
      fin,
      rsv,
      opcode,
      masked,
      masking_key,
      length,
      payload,
      data,        %% The unmasked payload. Makes payload redundant.
      ws_state     %% The ws_state after unframing this frame.
                       %% This is useful for the endpoint to know what type of
                       %% fragment a potentially fragmented message is.
     }).

%----------------------------------------------------------------------
% The state of a WebSocket connection.
% This is held by the ws owner process and passed in calls to yaws_api.
%----------------------------------------------------------------------
-type frag_type() :: text
           | binary
           | none.  %% The WebSocket is not expecting continuation
                            %% of any fragmented message.
-record(ws_state, {
          vsn :: integer(),           % WebSocket version number
          sock,                     % gen_tcp or gen_ssl socket
          frag_type :: frag_type(),
          endpoint :: client | server
     }).
