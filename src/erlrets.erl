%%
%% Copyright (C) 2015
%% This is a product for HomeNovate LLC
%% Authors: Jorge Garrido <zgbjgg@gmail.com>
%% All rights reserved.
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%%

-module(erlrets).

-behaviour(gen_server).

%% API
-export([start_link/0, search/1, subscribe/0, unsubscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {driver = [], subscription = []}).

-include("erlrets.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Send a search query to the libRETS 
%%
%% @spec search(Query :: string()) -> ok
%%--------------------------------------------------------------------
-spec search(Query :: string()) -> ok.
search(Query) ->
    gen_server:call(?MODULE, {search, Query}).

%%--------------------------------------------------------------------
%% @doc Subscribe pid to send data
%%
%% @spec subscribe() -> {ok, reference()}
%%--------------------------------------------------------------------
-spec subscribe() -> {ok, reference()}.
subscribe() ->
    gen_server:call(?MODULE, subscribe).

%%--------------------------------------------------------------------
%% @doc Unsubscribe pid via reference to avoid send data
%%
%% @spec unsubscribe( Ref :: reference()) -> {ok, unsubscribe}
%%--------------------------------------------------------------------
-spec unsubscribe(Ref :: reference()) -> {ok, unsubscribe}.
unsubscribe(Ref) ->
    gen_server:call(?MODULE, {unsubscribe, Ref}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{driver=?DRIVER_SO_NAME, subscription = []}}.

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
handle_call({search, Query}, _From, State=#state{driver=NifDriver})            ->
    lager:info("executing search query: ~s on drv_nif: ~p~n", [Query, NifDriver]), 
    {reply, {ok, ok}, State};    
handle_call(subscribe, {Pid, _}, #state{driver=Port, subscription=S})          ->
    Reference = erlang:make_ref(),
    {reply, {ok, Reference}, #state{driver=Port, subscription=[ {Reference, Pid} | S]}};
handle_call({unsubscribe, Ref}, _From, #state{driver=Port, subscription=S})    ->
    NewS = [ Scr || {Reference, _}=Scr <- S, Ref =/= Reference],
    {reply, {ok, unsubscribe},  #state{driver=Port, subscription=NewS}};    
handle_call(stop, _From, State=#state{driver=Port})                            ->
    true = erlang:port_close(Port),
    {stop, normal, ok, State}.

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
handle_info({'EXIT', _Port, _Reason}, _State) 				      ->
    exit(port_terminated),
    {noreply, #state{driver=nil, subscription=[]}};
handle_info({_Port, closed}, _State) 					      ->
    {noreply, #state{driver=nil, subscription=[]}}.

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
