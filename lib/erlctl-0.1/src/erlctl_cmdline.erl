-module (erlctl_cmdline).
-export([process_cmdline/0]).

% Command Line Handling Functions
process_cmdline() ->
  % Get Script Name and Args
  [ScriptName | Args0] = init:get_plain_arguments(),
  RunName = filename:basename(ScriptName),
  % Process Options and Arguments
  {ok,Opts0,Args1} = try
    handle_arg(Args0,[])
  catch
    error:{sys_arg,[BadArg | Rst0] } ->
      io:format(standard_error,"bad system argument: ~p ~p~n",[BadArg, Rst0]),
      erlctl_err:halt_with_error();
    error:badmatch ->
      io:format(standard_error,"error processing system arguments!~n",[]),
      erlctl_err:halt_with_error()
  end,
  % Infer Usage From Name of Control Script and Args
  case {lists:reverse(RunName),Args1} of
    {"ltclre",[C0, C1 | Rst1]} ->        % erlctl <app> <cmd> [args]
      AppName = C0,
      Cmd = C1,
      Args2 = Rst1;
    {"ltc_" ++ RevName, [C0 | Rst1]} ->  % <app>_ctl <cmd> [args]
      AppName = lists:reverse(RevName),
      Cmd = C0,
      Args2 = Rst1;
    {"ltc" ++ RevName, [ C0 | Rst1 ]} -> % <app>ctl <cmd> [args]
      AppName = lists:reverse(RevName),
      Cmd = C0,
      Args2 = Rst1;
    {_, [ C0 | Rst1 ]} ->                % <app> <cmd> [args]
      AppName = RunName,
      Cmd = C0,
      Args2 = Rst1;
    _ ->
      AppName = error, Cmd = error, Args2 = [], % make vars safe
      io:format(standard_error,"Unable to parse app / command: ~p~n",[Args1]),
      erlctl_err:halt_with_error()
  end,
  Opts1 = [{app,AppName} | Opts0],
  Module = list_to_atom(AppName ++ "_cli"),
  Function = list_to_atom(Cmd),
  {ok,Opts1,Module,Function,Args2}.

% System Arguments
handle_arg(["-h",HostName | Rest ], Opts) ->
  handle_arg(Rest,[{host,HostName}   | Opts]);
handle_arg(["-l"          | Rest ], Opts) ->
  handle_arg(Rest,[{names,long}      | Opts]);
handle_arg(["-s"          | Rest ], Opts) ->
  handle_arg(Rest,[{names,short}     | Opts]);
handle_arg(["-N",FN       | Rest ], Opts) ->
  handle_arg(Rest,[{fullnode,FN}     | Opts]);
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

