-module (erlctl_err).
-export([format/1,format/2]).
-export([unknown_command/0,networking_failure/0,remote_error/1,
  cannot_start_vm/2, halt_with_error/0, bad_cmdline/2]).

bad_cmdline(Msg,Data) ->
  format("error processing command line: " ++ Msg ++ "~n",Data),
  halt(249).

unknown_command() ->
  format("unrecognized command~n",[]),
  halt(250).

networking_failure() ->
  format("Unable to start networking!~n",[]),
  halt(251).

remote_error(Data) ->
  format("Error on remote node: ~p~n",Data),
  halt(252).

cannot_start_vm(Msg,Data) ->
  format("Error starting 'erl': " ++ Msg ++ "~n",Data),
  halt(253).

halt_with_error() ->
  format("unspecified fatal error~n",[]),
  halt(255).

format(M) ->
  format(M,[]).

format(M,D) ->
  try
    io:format(standard_error,M,D)
  catch
    _:_ ->
      io:format(M,D)
  end.
