-module (erlctl_cmd).
-export([start/0]).

-include_lib("erlctl/include/internal.hrl").

% Entry Point for Running Commands
-spec( start() -> no_return() ).
start() ->
  ok = init(),
  {ok,Opts} = erlctl_cmdline:process_cmdline(),
  run_stage(Opts,always).

run_stage(Opts0,Stage0) ->
  try
    stage(Opts0,Stage0)
  of
    {ok,Opts1,Stage1} ->
      run_stage(Opts1,Stage1);
    wait ->
      receive
        never_comes -> never_exits
      end;
    _ ->
      erlctl_err:halt_with_error() % FIXME: Better Error
  catch
    _A:_B ->
      erlctl_err:halt_with_error() % FIXME: Better Error
  end.

init() ->
  % Clear Out Default Log Handlers
  lists:foreach(fun error_logger:delete_report_handler/1,
    gen_event:which_handlers(error_logger)),
  % Start Delegate
  erlctl_proc:start_delegate(),
  ok.

get_mfa(Opts) ->
  proplists:get_value(mfa,Opts).

stage(Opts,Stage) ->
  MFA = get_mfa(Opts),
  Where = where(Opts,Stage),
  Result = erlctl_proc:run(Where,Stage,MFA),
  handle_result(Opts,Stage,Result).

where(_Opts,always)      -> local;
where(_Opts,not_running) -> local;
where( Opts,running)     -> list_to_atom(proplists:get_value(target,Opts));
where( Opts,started)     -> list_to_atom(proplists:get_value(target,Opts)).

handle_result(Opts0,always,skip) ->
  {ok,Opts1} = erlctl_net:start_networking(Opts0),
  Node = list_to_atom(proplists:get_value(target,Opts1)),
  case net_adm:ping(Node) of
    pang ->
      {ok,Opts1,not_running};
    pong ->
      {ok,Opts1,running}
  end;

handle_result(Opts,Stage,restart) ->
  handle_result(Opts,Stage,{restart,[]});
handle_result(Opts,Stage,{restart,SOpts}) ->
  case erlctl_net:ensure(down,Opts) of
    down ->
      handle_result(Opts,Stage,{start,SOpts});
    up ->
      erlctl_err:cannot_start_vm("Timeout stopping server for restart",[])
  end;
handle_result(Opts,Stage,{restart,SOpts,Msg}) ->
  handle_result(Opts,Stage,{restart,SOpts,Msg,[]});
handle_result(Opts,Stage,{restart,SOpts,Msg,Data}) ->
  erlctl:format(Msg,Data),
  handle_result(Opts,Stage,{restart,SOpts});

handle_result(Opts,Stage,start) ->
  handle_result(Opts,Stage,{start,[]});
handle_result(Opts,_Stage,{start,SOpts}) ->
  {ok,Node} = erlctl_vm:start_vm(SOpts ++ Opts),
  {ok,[{node,Node} | Opts],started};
handle_result(Opts,Stage,{start,SOpts,Msg}) ->
  handle_result(Opts,Stage,{start,SOpts,Msg,[]});
handle_result(Opts,Stage,{start,SOpts,Msg,Data}) ->
  erlctl:format(Msg,Data),
  handle_result(Opts,Stage,{start,SOpts});
handle_result(_Opts,_Stage,skip) ->
  erlctl_err:unknown_command(),
  wait;

handle_result(_Opts,_Stage,ok) ->
  erlctl:exit_with_code(0),
  wait;
handle_result(Opts,Stage,{ok,Msg}) ->
  handle_result(Opts,Stage,{ok,Msg,[]});
handle_result(Opts,Stage,{ok,Msg,Data}) ->
  erlctl:format(Msg,Data),
  handle_result(Opts,Stage,ok);

handle_result(Opts,Stage,error) ->
  handle_result(Opts,Stage,{error,255});
handle_result(_Opts,_Stage,{error,N}) ->
  erlctl:exit_with_code(N),
  wait;
handle_result(Opts,Stage,{error,N,Msg}) ->
  handle_result(Opts,Stage,{error,N,Msg,[]});
handle_result(Opts,Stage,{error,N,Msg,Data}) ->
  erlctl:format(Msg,Data),
  handle_result(Opts,Stage,{error,N});

handle_result(Opts,Ctx,Other) ->
  Msg = "Unexpected Return from Stage ~p:~n  ~p~n",
  Data = [Ctx,Other],
  handle_result(Opts,Ctx,{error,254,Msg,Data}).
