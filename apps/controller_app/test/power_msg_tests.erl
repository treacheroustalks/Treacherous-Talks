%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%% @end
%%%-------------------------------------------------------------------
-module(power_msg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/game.hrl").
-include_lib("datatypes/include/message.hrl").

-export([tests/1, success/4]).

tests([Callback, SessId, GameId, Country]) ->
    [?_test(success(Callback, SessId, GameId, Country))].
%%-------------------------------------------------------------------
%% Get current games tests
%%-------------------------------------------------------------------
success(Callback, SessId, GameId, Country) ->
    ?debugMsg("MODERATOR MESSAGE TEST SUCCESS"),
    game_timer:sync_event(GameId, timeout),
    Msg = create_valid_message(Country, GameId),
    Cmd = {power_msg, {ok, SessId, Msg}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, _Game} = Result,
    ?assertEqual({power_msg, success}, CmdRes),
    ?debugMsg("MODERATOR MESSAGE TEST SUCCESS finished").


%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------
create_valid_message(Country, GameId) ->
      #frontend_msg{to = [Country],
                    game_id = GameId,
                    content = "\n\n    A sample message to nick player which
                              contain several line\n    have fun\n\n    "
                   }.
