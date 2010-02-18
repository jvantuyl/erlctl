-module (test_cli).
-include_lib("erlctl/include/erlctl.hrl").

%% @doc Prints out the installed version of the ctl_ex server.
version(none,[]) ->
  {ok,"Version: ~p~n",[0.1]}.

%% @doc Start the ctl_ex application.
start(running,_) ->
  {error,1,"Already running.~n"};
start(not_running,[]) ->
  {start,"Starting.~n"};
start(started,[]) ->
  ok = application:start(ctl_ex),
  erlctl:format("Started~n",[]),
  ok.

%% @doc Stop the ctl_ex application.
stop(not_running,[]) ->
  format("Not running.~n"),
  ok;
stop(running,[]) ->
  application:stop(ctl_ex),
  format("Stopping.~n"),
  server_exit(),
  ok.

%% @doc List the users.
list_users(not_running,_) ->
  format("Not running.~n"),
  {error,1};
list_users(running,[]) ->
  {ok,Users} = ctl_ex:list_users(),
  format("Users:~n"),
  lists:foreach(
    fun (User) ->
      format("  ~s~n",[User])
    end,
    Users
  ),
  ok.

%% @doc Add a user.
add_user(not_running,_) ->
  format("Not running.~n"),
  {error,1};
add_user(running,[User]) ->
  ok = ctl_ex:add_user(User),
  format("User added~n"),
  ok.

%% @doc Delete a user.
del_user(not_running,_) ->
  format("Not running.~n"),
  {error,1};
del_user(running,[User]) ->
  case ctl_ex:del_user(User) of
    ok ->
      format("User deleted~n"),
      ok;
    no_such_user ->
      format("Error: No Such User!~n"),
      {error,1}
  end.
