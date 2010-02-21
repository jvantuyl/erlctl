-module (test_cli).
-include_lib("erlctl/include/erlctl.hrl").

%% @doc Helper Function for Usage Information
usage() ->
  Opts =   erlctl:get_opts(),
  App =    proplists:get_value(app,Opts),
  Script = proplists:get_value(script,Opts),
  [
    {"Usage for ~s:",[App]},
    {"  ~s <command> ...",[Script]},
     "",
     "Commands:",
    {"  start:            start the ~s daemon",[App]},
    {"  stop:             stop the ~s daemon",[App]},
    {"  restart:          restart the ~s daemon",[App]},
     "  version:          display version information",
     "  lists_users:      lists users",
     "  add_user <user>:  adds a user",
     "  del_user <user>:  deletes a user"
  ].

%% @doc Prints out help for the ctl_ex application
help(always,[]) ->
  lists:foreach(
    fun
      ({E,D}) ->
        format(E ++ "~n",D);
      (E) ->
        format(E ++ "~n",[])
    end,
    usage()
  ),
  ok.

%% @doc Prints out the installed version of the ctl_ex server.
version(always,[]) ->
  {ok,"Version: ~p",[0.1]}.

%% @doc Start the ctl_ex application.
start(running,_) ->
  {error,1,"Already running."};
start(not_running,[]) ->
  {start,[],"Starting."};
start(started,[]) ->
  ok = application:start(ctl_ex),
  erlctl:format("Started!~n",[]),
  ok.

%% @doc Stop the ctl_ex application.
stop(not_running,[]) ->
  {ok,"Not running."};
stop(running,[]) ->
  application:stop(ctl_ex),
  format("Stopping.~n"),
  server_exit(),
  ok.

%% @doc Restart the ctl_ex application
restart(not_running,X) ->
  start(not_running,X);
restart(running,X) ->
  stop(running,X),
  {restart,[],"Restarting..."};
restart(started,X) ->
  start(started,X).

%% @doc List the users.
list_users(not_running,_) ->
  {error,1,"Not running."};
list_users(running,[]) ->
  {ok,Users} = ctl_ex:list_users(),
  format("Users:~n"),
  case Users of
    [] ->
      io:format("  <no users>~n");
    _ ->
      lists:foreach(
        fun (User) ->
          format("  ~s~n",[User])
        end,
        Users
      )
  end,
  ok.

%% @doc Add a user.
add_user(not_running,_) ->
  {error,1,"Not running."};
add_user(running,[User]) ->
  case ctl_ex:add_user(User) of
    ok ->
      {ok,"User ~s added",[User]};
    user_exists ->
      {error,1,"User ~s already exists",[User]}
  end.

%% @doc Delete a user.
del_user(not_running,_) ->
  {error,1,"Not running."};
del_user(running,[User]) ->
  case ctl_ex:del_user(User) of
    ok ->
      {ok,"User ~s deleted",[User]};
    no_such_user ->
      {error,1,"No such user: ~s",[User]}
  end.
