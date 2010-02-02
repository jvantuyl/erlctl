-module (erlctl).
-export([format/1,format/2,exit_with_code/1]).

% FIXME: These are dummy functions!

format(Disp) -> format(Disp,[]).
format(Disp,Opts) ->
  io:format(Disp,Opts).

exit_with_code(N) ->
  io:format("Exited with status ~p!~n",[N]).
