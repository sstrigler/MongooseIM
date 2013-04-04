%%% @author Stefan Strigler <stefan.strigler@erlang-solutions.com>
%%% @copyright (C) 2013, Stefan Strigler
%%% @doc
%%%
%%% @end
%%% Created :  4 Apr 2013 by Stefan Strigler <stefan.strigler@erlang-solutions.com>
-module(ejabberd_auth_webhooks).
-author('stefan@strigler.de').

%% External exports
-export([start/1,
         set_password/3,
         check_password/3,
         check_password/5,
         try_register/3,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_password/2,
         get_password_s/2,
         is_user_exists/2,
         remove_user/2,
         remove_user/3,
         plain_password_required/0
        ]).

-include("ejabberd.hrl").
-include("mod_cce.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) ->
    ok.

plain_password_required() ->
    true.

check_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    ejabberd_hooks:run_fold(
      check_password, LServer,
      {LUser, LServer, Password}).

check_password(User, Server, Password, _StreamID, _Digest) ->
    check_password(User, Server, Password).

set_password(User, Server, Password) ->
    false.

try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

dirty_get_registered_users() ->
    [].

get_vh_registered_users(_Server) ->
    [].

get_password(_User, _Server) ->
    false.

get_password_s(_User, _Server) ->
    "".

is_user_exists(User, Server) ->
    true.

remove_user(_User, _Server) ->
    {error, not_allowed}.

remove_user(_User, _Server, _Password) ->
    not_allowed.
