-module (test_cli).
-import(erlctl,[format/1,format/2,exit_with_code/1,server_exit/0]).
-export([version/2,start/2,stop/2,list_users/2,add_user/2,del_user/2]).

%% @doc Prints out the installed version of the ctl_ex server.
version(none,[]) ->
  format("Version: ~p~n",[0.1]),
  exit_with_code(0).

%% @doc Start the ctl_ex application.
start(running,_) ->
  format("Already running.~n"),
  exit_with_code(1);
start(start,[]) ->
  ok = application:start(ctl_ex),
  erlctl:format("Started~n",[]),
  exit_with_code(0).

%% @doc Stop the ctl_ex application.
stop(not_running,[]) ->
  format("Not running.~n"),
  exit_with_code(0);
stop(running,[]) ->
  application:stop(ctl_ex),
  format("Stopping.~n"),
  server_exit(),
  exit_with_code(0).

%% @doc List the users.
list_users(not_running,_) ->
  format("Not running.~n"),
  exit_with_code(1);
list_users(running,[]) ->
  {ok,Users} = ctl_ex:list_users(),
  format("Users:~n"),
  lists:foreach(
    fun (User) ->
      format("  ~s~n",[User])
    end,
    Users
  ),
  exit_with_code(0).

%% @doc Add a user.
add_user(not_running,_) ->
  format("Not running.~n"),
  exit_with_code(1);
add_user(running,[User]) ->
  ok = ctl_ex:add_user(User),
  format("User added~n"),
  exit_with_code(0).

%% @doc Delete a user.
del_user(not_running,_) ->
  format("Not running.~n"),
  exit_with_code(1);
del_user(running,[User]) ->
  case ctl_ex:del_user(User) of
    ok ->
      format("User deleted~n"),
      exit_with_code(0);
    no_such_user ->
      format("Error: No Such User!~n"),
      exit_with_code(1)
  end.
