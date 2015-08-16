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
-module(erlrets_driver_nif).

-author('zgbjgg@gmail.com').

-export([login/1, 
         check_session/1]).

-include("erlrets.hrl").

-on_load(init/0).

init() ->
    Ebin = filename:dirname(code:which(?MODULE)),
    NifDir = filename:join(filename:dirname(Ebin), "priv"),
    lager:info("loading driver on ~p ~n", [NifDir ++ "/erl_rets_nif"]),
    case erlang:system_info(smp_support) of
        true  ->
            case erlang:load_nif(NifDir ++ "/erl_rets_nif", 0) of
                ok                    ->
                    lager:info("successfully loaded driver: ~s~n", [?DRIVER_SO_NAME]),
                    ok;
                {error, {reload, _}}  ->
                    lager:error("reloaded driver: ~s~n", [?DRIVER_SO_NAME]),
                    ok;
                {error, {upgrade, _}} ->
                    lager:error("upgrade driver: ~s~n", [?DRIVER_SO_NAME]),
                    ok
            end;
        false ->
            exit(no_smp_support)
    end.

%% @doc Try a login againts a RETS server using a query,
%% the query must be built with a erl interface.
%% @spec login(_Query :: term()) -> {ok, integer()}
-spec login(_Query :: term()) -> {ok, integer()}.
login(_Query) ->
    exit(nif_library_not_loaded).

%% @doc Validates if session is opened or not
%% @spec check_session(_Resource :: term()) -> active | expired
-spec check_session(_Resource :: term()) -> active | expired.
check_session(_Resource) ->
    exit(nif_library_not_loaded).
