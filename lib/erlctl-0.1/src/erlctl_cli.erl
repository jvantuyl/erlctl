-module (erlctl_cli).
-export([run_command/1,ensure_exit/0]).
-include_lib("kernel/include/inet.hrl"). % for #hostent

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

% Entry Point for Running Commands
run_command([ScriptName]) ->
  Name = filename:basename(ScriptName),
  {ok,RawOpts,CmdLine} = process_opts(),
  {ok,AppName,Cmd,Args} = split_cmdline(Name,CmdLine),
  Opts = [{app,AppName} | RawOpts],
  Module = list_to_atom(AppName ++ "_cli"),
  Function = list_to_atom(Cmd),
  erlctl:start_delegate(),
  try_context(none,Module,Function,Args,Opts),
  start_networking(Opts).

% Context Helpers
get_fqdn(HostName) ->
  case inet:gethostbyname(HostName) of
    {ok,#hostent{h_name = CName,h_aliases = Aliases}} ->
      Names = [CName | Aliases],
      hd(
        lists:filter(
          fun (X) ->
            not lists:prefix("localhost",X)
          end,
          Names
        )
      );
    {error,_} ->
      HostName % fallback to short name
  end.

is_longname(Name) -> lists:member($.,Name).

get_shortname(undefined) ->
  {ok,HN} = inet:gethostname(),
  HN;
get_shortname(Manual) ->
  Manual.

get_longname(undefined) ->
  {ok,HN} = inet:gethostname(),
  get_fqdn(HN);
get_longname(Manual) ->
  case is_longname(Manual) of
    true ->
      Manual;
    false ->
      get_fqdn(Manual)
  end.

get_hostname(Opts) ->
  HnOpt = proplists:get_value(host,Opts),
  LnOpt = proplists:get_bool(longnames,Opts),
  case LnOpt of
    true ->
      HostName = get_shortname(HnOpt);
    false ->
      HostName = get_longname(HnOpt)
  end,
  case {LnOpt,is_longname(HostName)} of
    {false,true} ->
      io:format(standard_error,
        "Warning: using name with dot as a shortname~n",[]);
    {true,false} ->
      io:format(standard_error,
        "Warning: using name without a dot as a longname~n",[]);
    _ ->
      ok
  end,
  HostName.

cli_nodename(HostName) ->
  AppName = proplists:get_value(app),
  AppName ++ "ctl_" ++ os:getpid() ++ "@" ++ HostName.

start_networking(Opts) ->
  LongNames = proplists:get_bool(longnames,Opts),
  HostName = get_hostname(Opts),
  io:format("~p,~p~n",[LongNames,HostName]).

try_context(Ctx,Mod,Func,Args,_Opts) ->
  try apply(Mod,Func,[Ctx,Args]) of
    _ ->
      erlctl:exit_with_code(0)
  catch
    error:undef ->
      no_command
  end.

% Various Error Conditions

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
