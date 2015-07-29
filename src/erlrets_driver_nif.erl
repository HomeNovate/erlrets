-module(erlrets_driver_nif).

-export([login/1]).

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

-spec login(Query :: binary()) -> tuple().

%% @doc Try a login againts a RETS server using a query,
%% the query must be built with a erl interface.

login(_Query) ->
    exit(nif_library_not_loaded).
