-module (erlctl_cli).
-export([help/2]).

help(always,[]) ->
  erlctl:format("Usage: erlctl <app> [<command> ...]~n"),
  erlctl:exit_with_code(1).
