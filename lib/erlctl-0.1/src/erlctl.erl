-module (erlctl).
-export([start/1,start_ack/1]).
-export([format/1,format/2,exit_with_code/1,server_exit/0]).

start([NotifyNode]) ->
  ok = application:start(sasl),
  ok = application:start(erlctl),
  spawn(NotifyNode,erlctl,start_ack,[node()]).

start_ack(StartedNode) ->
  erlctl_cmd_runner ! {vm_started,StartedNode}.

exit_with_code(Code) ->
  erlctl_proc:send({halt,Code}),
  timer:sleep(100), % to prevent the exit signal from beating the message
  ok.

server_exit() ->
  timer:apply_after(50,init,stop,[]).

format(Format) ->
  erlctl_proc:send({format,Format,[]}),
  ok.

format(Format,Data) ->
  erlctl_proc:send({format,Format,Data}),
  ok.
