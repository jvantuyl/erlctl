-module (erlctl_cli).
-export([usage/2]).

usage(none,[]) ->
  erlctl:format("Usage: erlctl <app> [<command> ...]~n"),
  erlctl:exit_with_code(1).