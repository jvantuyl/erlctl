-module (erlctl_cli).
-export([run_command/1,ensure_exit/0]).
-include_lib("kernel/include/inet.hrl"). % for #hostent
-define(DEF_NAMES,long).
-define(STARTUP_DELAY,5000).

% Command Line Handling Functions
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

% Entry Point for Running Commands
run_command([ScriptName]) ->
  setup_node(),
  Name = filename:basename(ScriptName),
  {ok,Opts0,CmdLine} = process_opts(),
  {ok,AppName,Cmd,Args} = split_cmdline(Name,CmdLine),
  Opts1 = [{app,AppName} | Opts0],
  Module = list_to_atom(AppName ++ "_cli"),
  Function = list_to_atom(Cmd),
  erlctl:start_delegate(),
  try_context(none,Module,Function,Args,Opts1),
  {ok,Opts2} = start_networking(Opts1),
  try_remote(running,Module,Function,Args,Opts2),
  try_context(not_running,Module,Function,Args,Opts2),
  try_start(Module,Function,Args,Opts2),
  not_found().

setup_node() ->
  register(erlctl_cmd_runner,self()), % Used By Started Nodes
  lists:foreach(fun error_logger:delete_report_handler/1,
    gen_event:which_handlers(error_logger)), % No error reports needed
  ok.

try_remote(Ctx,Module,Function,Args,Opts) ->
  Delegate = erlctl:get_delegate(),
  Target = list_to_atom(proplists:get_value(target,Opts)),
  process_flag(trap_exit,true),
  MFA = {Module,Function,[Ctx,Args]},
  Pid = spawn_link(Target,erlctl,remote_run,[Delegate,MFA]),
  receive
    {'EXIT',Pid,noconnection} ->
      no_vm;
    X ->
      remote_error([X])
  end,
  process_flag(trap_exit,false),
  next.

try_start(Module,Function,Args,Opts) ->
  TgtName = proplists:get_value(target,Opts),
  TgtNode = list_to_atom(TgtName),
  case proplists:get_value(names,Opts,?DEF_NAMES) of
    long ->
      NameType = "-name";
    short ->
      NameType = "-sname"
  end,
  ErlPath = case os:find_executable("erl") of
    false ->
      cannot_start_vm("cannot find executable",[]);
    FoundPath ->
      FoundPath
  end,
  NameArgs = [NameType,TgtName],
  DaemonArgs = ["-detached","-noshell","-mode","interactive"],
  StartArgs =  ["-s","erlctl","start",atom_to_list(node())],
  ErlOpts = [
    {args, NameArgs ++ DaemonArgs ++ StartArgs},
    exit_status,hide
  ],
  Port = open_port({spawn_executable,ErlPath},ErlOpts),
  receive
    {Port,{exit_status,0}} ->
      started;
    {Port,{exit_status,X}} ->
      cannot_start_vm("exited with error ~p",[X])
  end,
  receive
    {vm_started,TgtNode} ->
      Node = TgtNode,
      ok;
    {vm_started,ActualNode} ->
      Node = ActualNode,
      io:format(standard_error,"Unexpected VM name ~p~n",[ActualNode])
    after ?STARTUP_DELAY ->
      Node = undefined,
      cannot_start_vm("timeout waiting for VM to start",[])
  end,
  Opts2 = [ {node,Node} | Opts ],
  try_remote(start,Module,Function,Args,Opts2),
  next.

% Context Helpers
is_longname(Name) -> lists:member($.,Name).

make_hostname(short,auto) ->
  {ok,HN} = inet:gethostname(),
  HN;
make_hostname(short,Manual) ->
  Manual;
make_hostname(long,auto) ->
  {ok,HN} = inet:gethostname(),
  DN = inet_db:res_option(domain), % Networking Must Be Running Here!
  HN ++ "." ++ DN;
make_hostname(long,Manual) ->
  case is_longname(Manual) of
    true ->
      Manual;
    false ->
      DN = inet_db:res_option(domain),
      Manual ++ "." ++ DN % Networking Must Be Running Here!
  end.

get_hostname(Opts) ->
  NmOpt = proplists:get_value(names,Opts,?DEF_NAMES),
  HnOpt = proplists:get_value(host,Opts,auto),
  HostName = make_hostname(NmOpt,HnOpt),
  case {NmOpt,is_longname(HostName)} of
    {short,true} ->
      io:format(standard_error,
        "Warning: using name with dot as a shortname (~p)~n",[HostName]),
        networking_failure();
    {long,false} ->
      io:format(standard_error,
        "Warning: using name without a dot as a longname (~p)~n",[HostName]),
        networking_failure();
    _ ->
      ok
  end,
  HostName.

cli_nodename(Opts) ->
  AppName = proplists:get_value(app,Opts),
  list_to_atom(AppName ++ "ctl_" ++ os:getpid()).

svr_nodename(Opts) ->
  case proplists:get_value(fullnode,Opts) of
    undefined ->
      HostName = get_hostname(Opts),
      case proplists:get_value(node,Opts) of
        undefined ->
          NodeName = proplists:get_value(app,Opts);
        NName ->
          NodeName = NName
      end,
      NodeName ++ "@" ++ HostName;
    NodeName ->
      NodeName % FIXME: Verify fully specified node names?
  end.

start_networking(Opts) ->
  case proplists:get_value(names,Opts,?DEF_NAMES) of
    long -> Names = longnames;
    short -> Names = shortnames
  end,
  CN = cli_nodename(Opts),
  case net_kernel:start([CN,Names]) of
    {ok,_} ->
      ok;
    {error,_} ->
      networking_failure()
  end,
  SvrNode = svr_nodename(Opts),
  {ok,[{target,SvrNode} | Opts]}.

try_context(Ctx,Mod,Func,Args,_Opts) ->
  try apply(Mod,Func,[Ctx,Args]) of
    _ ->
      erlctl:exit_with_code(0)
  catch
    error:function_clause ->
      no_command;
    error:undef ->
      no_command
  end.

% Various Error Conditions

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

% This is called if run_command doesn't terminate the system, which shouldn't
% ever happen unless there is a critical error.
ensure_exit() ->
  io:format(standard_error,"error executing command~n",[]),
  halt(254).

halt_with_error() ->
  io:format(standard_error,"unspecified fatal error~n",[]),
  halt(255).

