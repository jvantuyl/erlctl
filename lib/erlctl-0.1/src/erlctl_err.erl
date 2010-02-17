-module (erlctl_err).
-export([not_found/0,networking_failure/0,remote_error/1,cannot_start_vm/2,
  halt_with_error/0]).

not_found() ->
  io:format(standard_error,"unrecognized command~n",[]),
  halt(250).

networking_failure() ->
  io:format(standard_error,"Unable to start networking!~n",[]),
  halt(251).

remote_error(Data) ->
  io:format(standard_error,"Error on remote node: ~p~n",Data),
  halt(252).

cannot_start_vm(Msg,Data) ->
  io:format(standard_error,"Error starting 'erl': " ++ Msg ++ "~n",Data),
  halt(253).

halt_with_error() ->
  io:format(standard_error,"unspecified fatal error~n",[]),
  halt(255).
