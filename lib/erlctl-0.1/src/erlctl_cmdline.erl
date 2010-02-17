-module (erlctl_cmdline).
-export([process_cmdline/0]).

% Command Line Handling Functions
process_cmdline() ->
  % Get Script Name and Args
  [ScriptName | Args0] = init:get_plain_arguments(),
  RunName = filename:basename(ScriptName),
  % Process Options and Arguments
  {ok,Opts0,Args1} = try
    handle_sysargs(Args0,[])
  catch
    error:{sysarg,[BadArg | Rst0] } ->
      io:format(standard_error,"bad system argument: ~p ~p~n",[BadArg, Rst0]),
      erlctl_err:halt_with_error();
    error:badmatch ->
      io:format(standard_error,"error processing system arguments!~n",[]),
      erlctl_err:halt_with_error()
  end,
  % Infer Usage From Name of Control Script and Args
  case normalize(RunName,Args1) of
    % "erlctl", show generic usage
    [] ->
      AppName = "erlctl", Cmd = "usage", Args2 = [];
    % "erlctl <app>" or "<app>ctl", show app usage
    [App] ->
      AppName = App, Cmd = "usage", Args2 = [];
    % "erlctl <app> <cmd> ..." or "<app>ctl <cmd> ...", run command
    [App, Command | CArgs] ->
      AppName = App, Cmd = Command, Args2 = CArgs
  end,
  Opts1 = [{app,AppName} | Opts0],
  Module = list_to_atom(AppName ++ "_cli"),
  Function = list_to_atom(Cmd),
  {ok,Opts1,Module,Function,Args2}.

normalize("erlctl",Args) -> % Strip off "erlctl"
  Args;
normalize(Name,Args) -> % Convert "<app>_?ctl" into "<app>"
  Len = length(Name),
  case lists:suffix("ctl",Name) of
    true ->
      case lists:suffix("_ctl",Name) of
        true ->
          [ lists:sublist(Name,Len - 4) | Args ];
        false ->
          [ lists:sublist(Name,Len - 3) | Args ]
      end;
    false ->
      [Name | Args]
  end.

% System Arguments
handle_sysargs(["-h",HostName | Rest ], Opts) ->
  handle_sysargs(Rest,[{host,HostName}   | Opts]);
handle_sysargs(["-l"          | Rest ], Opts) ->
  handle_sysargs(Rest,[{names,long}      | Opts]);
handle_sysargs(["-s"          | Rest ], Opts) ->
  handle_sysargs(Rest,[{names,short}     | Opts]);
handle_sysargs(["-N",FN       | Rest ], Opts) ->
  handle_sysargs(Rest,[{fullnode,FN}     | Opts]);
handle_sysargs(["-n",NodeName | Rest ], Opts) ->
  handle_sysargs(Rest,[{node,NodeName}   | Opts]);
handle_sysargs(["-c",ConfFile | Rest ], Opts) ->
  handle_sysargs(Rest,[{config,ConfFile} | Opts]);
handle_sysargs([ [X | _] | _ ] = Args, Opts) when X =/= $- ->
  {ok,Opts,Args};
handle_sysargs([],Opts) ->
  {ok,Opts,[]};
handle_sysargs(Args, _Opts) ->
  erlang:error({sysarg,Args}).

