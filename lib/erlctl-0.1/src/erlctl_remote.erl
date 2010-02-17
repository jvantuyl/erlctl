-module (erlctl_remote).
-export([remote_command/6,run/2]).
-export([start_delegate/0,get_delegate/0,set_delegate/1]).

-include_lib("erlctl/include/internal.hrl").

remote_command(Node,Ctx,Module,Function,Args,_Opts) ->
  Delegate = get_delegate(),
  MFA = {Module,Function,[Ctx,Args]},
  process_flag(trap_exit,true),
  Pid = spawn_link(Node,erlctl_remote,run,[Delegate,MFA]),
  R = receive
    {'EXIT',Pid,noconnection} ->
      no_vm;
    {'EXIT',Pid,normal} ->
      erlctl:exit_with_code(0),
      exiting;
    X ->
      erlctl_err:remote_error([X]),
      exiting
  end,
  process_flag(trap_exit,false),
  R.

run(Delegate,{Module,Function,Args}) ->
  set_delegate(Delegate),
  apply(Module,Function,Args).

start_delegate() ->
  D = spawn_link(fun delegate/0),
  set_delegate(D),
  D.

get_delegate() ->
  get(delegate).

set_delegate(Delegate) ->
  put(delegate,Delegate),
  ok.

delegate() ->
  process_flag(trap_exit,true),
  delegate_loop().

delegate_loop() ->
  receive
    {format,Format,Data} ->
      io:format(Format,Data);
    {halt,Code} ->
      halt(Code);
    {'EXIT',_,_} ->
      halt(255)
  end,
  delegate_loop().

