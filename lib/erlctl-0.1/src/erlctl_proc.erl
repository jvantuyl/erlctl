-module (erlctl_proc).
-export([run/3,send/1,start_delegate/0]).
-export([master/4,blaster/3]).

-include_lib("erlctl/include/internal.hrl").

% API to spawn a master
%   This insulates the command loop from the vagaries of remote nodes.
run(Where,Ctx,{Module,Function,Args}) ->
  Parent = self(),
  Delegate = get_delegate(),
  MastArgs = [Parent,Delegate,Where,{Module,Function,[Ctx,Args]}],
  spawn_link(?MODULE,master,MastArgs),
  receive
    {result,Result} ->
      Result
  end.

% This middleman runs locally, and traps exits to handle distribution
master(Parent,Delegate,Where,{Module,Function,Args}) ->
  process_flag(trap_exit,true),
  BlastArgs = [self(),Delegate,{Module,Function,Args}],
  case Where of
    local ->
      spawn_link(?MODULE,blaster,BlastArgs);
    Node ->
      spawn_link(Node,?MODULE,blaster,BlastArgs)
  end,
  Returned = receive
    {result,Result} ->
      Result;
    no_command ->
      skip;
    {exception,Class,Type,Trace} ->
      {error,254,"Command Crashed: ~p:~p~nAt: ~p",[Class,Type,Trace]};
    {'EXIT',_,noconnection} ->
      {error,252, "Error Communicating With Remote Node ~p",[Where]};
    {'EXIT',_,Reason} ->
      {error,254,"Command Process Died Unexpectedly (due to ~p)",[Reason]}
  end,
  Parent ! {result,Returned},
  ok.

% This middleman runs where the command runs, it sets up the environment for
% the actual command and catches any exceptions
blaster(Master,Delegate,{M,F,A}) ->
  set_delegate(Delegate),
  Master ! try
    apply(M,F,A)
  of
    Result ->
      {result,Result}
  catch
    Class:Type ->
      [ _, Where | _ ] = Trace = erlang:get_stacktrace(),
      Here = {?MODULE,blaster,3},
      case {Class,Type,Where} of
        {error,undef,Here} ->
          no_command;
        {error,function_clause,Here} ->
          no_command;
        _ ->
          {exception,Class,Type,Trace}
      end
  end,
  ok.

start_delegate() ->
  D = spawn_link(fun delegate/0),
  set_delegate(D),
  register(cmd_delegate,D),
  D.

get_delegate() -> % Locally, it should be registered...
  case whereis(cmd_delegate) of
    undefined -> % Remotely, it's in the process dictionary
      get(delegate);
    Delegate when is_pid(Delegate) ->
      Delegate
  end.

set_delegate(Delegate) ->
  put(delegate,Delegate),
  ok.

% Sends a command to the delegate, waiting for reply
send(Msg) ->
  D = get_delegate(),
  R = make_ref(),
  D ! {req,{self(),R},Msg},
  receive
    {ack,R} ->
      ok
  end.

% Used To Send Replies From Delegate
ack({From,Seq}) ->
  From ! {ack,Seq}.

% Implemented the Delegate
delegate() -> % First, Trap Exits
  process_flag(trap_exit,true),
  delegate_loop().

delegate_loop() -> % Then, loop over commands
  receive
    {req,Tag,Cmd} ->
      case Cmd of
        {format,Format,Data} ->
          io:format(Format,Data),
          ack(Tag);
        {halt,Code} ->
          halt(Code)
      end;
    {'EXIT',_,_} ->
      erlang:exit()
  end,
  delegate_loop().
