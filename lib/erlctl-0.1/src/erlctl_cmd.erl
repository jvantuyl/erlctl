-module (erlctl_cmd).
-export([start/0]).

-include_lib("kernel/include/inet.hrl"). % for #hostent

-define(DEF_NAMES,long).
-define(STARTUP_DELAY,5000).

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
      halt_with_error();
    error:badmatch ->
      io:format(standard_error,"error processing system arguments!~n",[]),
      halt_with_error()
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
      halt_with_error()
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

% Entry Point for Running Commands
-spec( start() -> no_return() ).
start() ->
  try
    ok = init(),
    {ok,Opts0,Module,Function,Args} = process_cmdline(),
    try_command(none,        Module,Function,Args,Opts0),
    {ok,Opts1} = start_networking(Opts0),
    try_command(running,     Module,Function,Args,Opts1),
    try_command(not_running, Module,Function,Args,Opts1),
    try_command(start,       Module,Function,Args,Opts1),
    not_found()
  catch
    _:_ ->
      %ST = erlang:get_stacktrace(),
      %io:format(standard_error,"WAHHHH! ~p:~p~n  ~p~n",[A,B,ST]),
      halt_with_error()
  end,
  never_reached.

init() ->
  % Clear Out Default Log Handlers
  lists:foreach(fun error_logger:delete_report_handler/1,
    gen_event:which_handlers(error_logger)),
  % Start Delegate
  erlctl:start_delegate(),
  ok.

try_command(running,Module,Function,Args,Opts) ->
  Node = list_to_atom(proplists:get_value(target,Opts)),
  case remote_command(Node,running,Module,Function,Args,Opts) of
    no_vm ->
      next
  end;
try_command(start,Module,Function,Args,Opts0) ->
  {ok,Node} = start_vm(Opts0),
  Opts1 = [ {node,Node} | Opts0 ],
  case remote_command(Node,start,Module,Function,Args,Opts1) of
    no_vm ->
      cannot_start_vm("Unable to start command in new VM",[]);
    exiting ->
      timer:sleep(?STARTUP_DELAY), % Wait for exit message
      erlctl:exit_with_code(0)
  end;
try_command(Ctx,Module,Function,Args,Opts) ->
  local_command(Ctx,Module,Function,Args,Opts).

remote_command(Node,Ctx,Module,Function,Args,_Opts) ->
  Delegate = erlctl:get_delegate(),
  MFA = {Module,Function,[Ctx,Args]},
  process_flag(trap_exit,true),
  Pid = spawn_link(Node,erlctl,remote_run,[Delegate,MFA]),
  R = receive
    {'EXIT',Pid,noconnection} ->
      no_vm;
    {'EXIT',Pid,normal} ->
      erlctl:exit_with_code(0),
      exiting;
    X ->
      remote_error([X]),
      exiting
  end,
  process_flag(trap_exit,false),
  R.

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

start_vm(Opts) ->
  % Build Target Node and Networking Info
  TgtName = proplists:get_value(target,Opts),
  TgtNode = list_to_atom(TgtName),
  case proplists:get_value(names,Opts,?DEF_NAMES) of
    long ->
      NameType = "-name";
    short ->
      NameType = "-sname"
  end,
  % Build VM Parameters
  NameArgs   = [NameType,TgtName],
  DaemonArgs = ["-detached","-noshell","-mode","interactive"],
  StartArgs  = ["-s","erlctl","start",atom_to_list(node())],
  Args = NameArgs ++ DaemonArgs ++ StartArgs,
  % Find VM Binary
  Path = case os:find_executable("erl") of
    false ->
      cannot_start_vm("cannot find executable",[]);
    FoundPath ->
      FoundPath
  end,
  % Register Process To Catch Started Message
  register(erlctl_cmd_runner,self()),
  % Open VM As Port
  Opts0 = [ {args,Args}, exit_status, hide ],
  Port = try
    open_port({spawn_executable,Path},Opts0)
  catch
    error:badarg ->
      Spawn = lists:flatten( [ [X,$ ] || X <- [Path | Args]] ),
      Opts1 = [ exit_status ],
      open_port({spawn,Spawn},Opts1)
  end,
  % Wait for It To Fail or Daemonize
  receive
    {Port,{exit_status,0}} ->
      started;
    {Port,{exit_status,X}} ->
      cannot_start_vm("exited with error ~p",[X])
  end,
  % Wait for Node To Report In
  receive
    {vm_started,TgtNode} ->
      {ok,TgtNode};
    {vm_started,ActualNode} ->
      io:format(standard_error,"Unexpected VM name ~p~n",[ActualNode]),
      {ok,ActualNode}
    after ?STARTUP_DELAY ->
      cannot_start_vm("timed out waiting for VM to start",[]),
      {error,timeout} % never reached
  end.

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

halt_with_error() ->
  io:format(standard_error,"unspecified fatal error~n",[]),
  halt(255).
