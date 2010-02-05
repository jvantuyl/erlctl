-module (erlctl).
-export([start_delegate/0,format/1,format/2,exit_with_code/1]).

start_delegate() ->
  Delegate = spawn_link(fun delegate/0),
  put(delegate,Delegate),
  Delegate.

delegate() ->
  process_flag(trap_exit,true),
  delegate_loop().

delegate_loop() ->
  receive
    {format,Format,Data} ->
      io:format(Format,Data);
    {halt,Code} ->
      halt(Code);
    {'EXIT',_,_} ->
      halt(255)
  end,
  delegate_loop().

exit_with_code(Code) ->
  Delegate = get(delegate),
  case node(Delegate) =:= node() of
    true ->
      halt(Code);
    false ->
      Delegate ! {halt,Code}
  end,
  ok.

format(Format) ->
  Delegate = get(delegate),
  case node(Delegate) =:= node() of
    true ->
      io:format(Format,[]);
    false ->
      Delegate ! {format,Format,[]}
  end,
  ok.

format(Format,Data) ->
  Delegate = get(delegate),
  case node(Delegate) =:= node() of
    true ->
      io:format(Format,Data);
    false ->
      Delegate ! {format,Format,Data}
  end,
  ok.
