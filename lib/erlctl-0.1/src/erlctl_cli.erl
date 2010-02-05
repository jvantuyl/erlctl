-module (erlctl_cli).
-export([run_command/1,ensure_exit/0]).

process_opts() ->
  Args = init:get_plain_arguments(),
  try handle_arg(Args,[])
  catch
    error:{sys_arg,[BadArg | Rest] } ->
      io:format(standard_error,"bad system argument: ~p ~p~n",[BadArg, Rest]),
      halt_with_error();
    error:badmatch ->
      io:format(standard_error,"error processing system arguments!~n",[]),
      halt_with_error()
  end.

handle_arg(["-h",HostName | Rest ], Opts) ->
  handle_arg(Rest,[{host,HostName}   | Opts]);
handle_arg(["-l"          | Rest ], Opts) ->
  handle_arg(Rest,[longnames         | Opts]);
handle_arg(["-s"          | Rest ], Opts) ->
  handle_arg(Rest,[shortnames        | Opts]);
handle_arg(["-n",NodeName | Rest ], Opts) ->
  handle_arg(Rest,[{node,NodeName}   | Opts]);
handle_arg(["-c",ConfFile | Rest ], Opts) ->
  handle_arg(Rest,[{config,ConfFile} | Opts]);
handle_arg([ [X | _] | _ ] = Args, Opts) when X =/= $- ->
  {ok,Opts,Args};
handle_arg([],Opts) ->
  {ok,Opts,[]};
handle_arg(Args, _Opts) ->
  erlang:error({sys_arg,Args}).

split_cmdline(RunName,Args) ->
  case {lists:reverse(RunName),Args} of
    {"ltclre",[C0, C1 | Rest]} ->        % erlctl <app> <cmd> [args]
      AppName = C0,
      Cmd = C1,
      CmdArgs = Rest;
    {"ltc_" ++ RevName, [C0 | Rest]} ->  % <app>_ctl <cmd> [args]
      AppName = lists:reverse(RevName),
      Cmd = C0,
      CmdArgs = Rest;
    {"ltc" ++ RevName, [ C0 | Rest ]} -> % <app>ctl <cmd> [args]
      AppName = lists:reverse(RevName),
      Cmd = C0,
      CmdArgs = Rest;
    {_, [ C0 | Rest ]} ->                % <app> <cmd> [args]
      AppName = RunName,
      Cmd = C0,
      CmdArgs = Rest;
    _ ->
      AppName = error, Cmd = error, CmdArgs = [], % make vars safe
      io:format(standard_error,"Unable to parse app or command: ~p~n",[Args]),
      halt_with_error()
  end,
  {ok,AppName,Cmd,CmdArgs}.

run_command([ScriptName]) ->
  Name = filename:basename(ScriptName),
  {ok,Opts,CmdLine} = process_opts(),
  {ok,AppName,Cmd,Args} = split_cmdline(Name,CmdLine),
  Module = list_to_atom(AppName ++ "_cli"),
  Function = list_to_atom(Cmd),
  erlctl:start_delegate(),
  context_none(Module,Function,Args,Opts).

try_func(Ctx,Mod,Func,Args) ->
  try
    {ok,apply(Mod,Func,[Ctx,Args])}
  catch
    error:undef -> no_func
  end.

context_none(Mod,Func,Args,_Opts) ->
  case try_func(none,Mod,Func,Args) of
    {ok,_} ->
      erlctl:exit_with_code(0);
    no_func ->
      not_found()
  end.

not_found() ->
  io:format("unrecognized command~n"),
  halt(250).

% This is called if run_command doesn't terminate the system, which shouldn't
% ever happen unless there is a critical error.
halt_with_error() ->
  io:format("unspecified fatal error~n"),
  halt(255).

ensure_exit() ->
  io:format("error executing command~n"),
  halt(254).
