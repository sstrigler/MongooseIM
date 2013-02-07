%%%-------------------------------------------------------------------
%%% @author Stefan Strigler <stefan.strigler@erlang-solutions.com>
%%% @copyright (C) 2013, Stefan Strigler
%%% @doc
%%%
%%% @end
%%% Created :  2 Feb 2013 by Stefan Strigler <stefan.strigler@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(mod_webhooks).

-behaviour(gen_mod).
-export([start/2,
         stop/1]).

-include("ejabberd.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the ejabberd module
%%
%% @spec start(Host, Opts) -> void()
%% @end
%%--------------------------------------------------------------------
start(Host, Opts) ->
    ?DEBUG("starting up for ~p", [Host]),
    Proc = gen_mod:get_module_proc(Host, ?SERVER),
    ChildSpec =
        {Proc,
         {?MODULE, start_link, [Host, Opts]},
         temporary,
         1000,
         worker,
         [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

%%--------------------------------------------------------------------
%% @doc
%% Stops the ejabberd module
%%
%% @spec stop(Host) -> void()
%% @end
%%--------------------------------------------------------------------
stop(Host) ->

    %% [TODO]
    %% unregister all routes

    Proc = gen_mod:get_module_proc(Host, ?SERVER),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?SERVER),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Host, Opts]) ->
    Endpoints = gen_mod:get_opt(endpoints, Opts, []),
    lists:foreach(fun(Uri) ->
                          get_configuration(Host, Uri)
                  end, Endpoints),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_configuration(Host, Uri) ->
    ?DEBUG("getting configuration from ~p", [Uri]),
    case ibrowse:send_req(Uri, [], get) of
        {ok, "200", _Header, Body} ->
            ?DEBUG("got result: ~p", [Body]),
            case json:decode(string:strip(Body, both, $\n)) of
                {ok, {Hooks}} ->
                    ?DEBUG("got hooks ~p", [Hooks]),
                    lists:foreach(fun(Hook) ->
                                          ?DEBUG("got hook ~p", [Hook]),
                                          register_hook(Host, Hook)
                                  end, Hooks);
                _JSONError ->
                    ?ERROR_MSG("failed to parse as json: ~p", [_JSONError])
            end;
        _IBrowseError ->
            ?ERROR_MSG("Got bad result for ~s:~n~p", [Uri, _IBrowseError])
    end.

register_hook(Host, {Hook, Uri}) ->
    ejabberd_hooks:add(binary_to_existing_atom(Hook, utf8), Host,
                       create_callback(binary:bin_to_list(Uri)), 50).

create_callback(Uri) ->
    ?DEBUG("creating callback for ~s", [Uri]),
    %% let's hope all callbacks are of this signature
    fun(User, Server, Resource, Stanza) ->
            ?DEBUG("callback called for uri ~s", [Uri]),
            {ok, Body} = json:encode({[{user, User}, {server, Server},
                                       {resource, Resource},
                                       {stanza, exml:to_binary(Stanza)}]}),
            ?DEBUG("Created JSON ~p", [Body]),
            ibrowse:send_req(Uri, [{"content-type", 
                                    "application/x-www-form-urlencoded"}],
                             post, "json="++Body)
    end.
