-module (erlctl).
-export([start/1,start_ack/1,remote_run/2]).
-export([start_delegate/0,get_delegate/0,set_delegate/1]).
-export([format/1,format/2,exit_with_code/1,server_exit/0]).

start([NotifyNode]) ->
  ok = application:start(sasl),
  ok = application:start(erlctl),
  spawn(NotifyNode,erlctl,start_ack,[node()]).

start_ack(StartedNode) ->
  erlctl_cmd_runner ! {vm_started,StartedNode}.

start_delegate() ->
  D = spawn_link(fun delegate/0),
  set_delegate(D),
  D.

get_delegate() ->
  get(delegate).

set_delegate(Delegate) ->
  put(delegate,Delegate),
  ok.

remote_run(Delegate,{Module,Function,Args}) ->
  set_delegate(Delegate),
  apply(Module,Function,Args).

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

exit_with_code(Code) ->
  Delegate = get_delegate(),
  case node(Delegate) =:= node() of
    true ->
      halt(Code);
    false ->
      unlink(Delegate),
      Delegate ! {halt,Code}
  end,
  timer:sleep(100), % to prevent the exit signal from beating the message
  ok.

server_exit() ->
  timer:apply_after(50,init,stop,[]).

format(Format) ->
  Delegate = get_delegate(),
  case node(Delegate) =:= node() of
    true ->
      io:format(Format,[]);
    false ->
      Delegate ! {format,Format,[]}
  end,
  ok.

format(Format,Data) ->
  Delegate = get_delegate(),
  case node(Delegate) =:= node() of
    true ->
      io:format(Format,Data);
    false ->
      Delegate ! {format,Format,Data}
  end,
  ok.
