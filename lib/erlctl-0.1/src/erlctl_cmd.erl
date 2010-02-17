-module (erlctl_cmd).
-export([start/0]).

-include_lib("erlctl/include/erlctl.hrl").

% Entry Point for Running Commands
-spec( start() -> no_return() ).
start() ->
  try
    ok = init(),
    {ok,Opts0,Module,Function,Args} = erlctl_cmdline:process_cmdline(),
    try_command(none,        Module,Function,Args,Opts0),
    {ok,Opts1} = erlctl_net:start_networking(Opts0),
    try_command(running,     Module,Function,Args,Opts1),
    try_command(not_running, Module,Function,Args,Opts1),
    try_command(start,       Module,Function,Args,Opts1),
    erlctl_err:not_found()
  catch
    _A:_B ->
      %ST = erlang:get_stacktrace(),
      %io:format(standard_error,"WAHHHH! ~p:~p~n  ~p~n",[A,B,ST]),
      erlctl_err:halt_with_error()
  end,
  never_reached.

init() ->
  % Clear Out Default Log Handlers
  lists:foreach(fun error_logger:delete_report_handler/1,
    gen_event:which_handlers(error_logger)),
  % Start Delegate
  erlctl_remote:start_delegate(),
  ok.

try_command(running,Module,Function,Args,Opts) ->
  Node = list_to_atom(proplists:get_value(target,Opts)),
  case erlctl_remote:remote_command(Node,running,Module,Function,Args,Opts) of
    no_vm ->
      next
  end;
try_command(start,Module,Function,Args,Opts0) ->
  {ok,Node} = erlctl_vm:start_vm(Opts0),
  Opts1 = [ {node,Node} | Opts0 ],
  case erlctl_remote:remote_command(Node,start,Module,Function,Args,Opts1) of
    no_vm ->
      erlctl_err:cannot_start_vm("Unable to start command in new VM",[]);
    exiting ->
      timer:sleep(?STARTUP_DELAY), % Wait for exit message
      erlctl:exit_with_code(0)
  end;
try_command(Ctx,Module,Function,Args,Opts) ->
  local_command(Ctx,Module,Function,Args,Opts).

local_command(Ctx,Mod,Func,Args,_Opts) ->
  try apply(Mod,Func,[Ctx,Args]) of
    _ ->
      erlctl:exit_with_code(0)
  catch
    error:function_clause ->
      no_command;
    error:undef ->
      no_command
  end.
