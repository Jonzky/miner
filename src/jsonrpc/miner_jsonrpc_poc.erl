-module(miner_jsonrpc_poc).

-include("miner_jsonrpc.hrl").
-behavior(miner_jsonrpc_handler).
-export([handle_rpc/2]).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"poc_find">>, #{ <<"key">> := Key }) ->

    POCID = blockchain_utils:poc_id(Key),
    OnionKeyHash = crypto:hash(sha256, Key),
    Ledger = blockchain:ledger(blockchain:blockchain()),
    case blockchain_ledger_v1:find_pocs(OnionKeyHash, Ledger) of
        {error, not_found} ->
            not_found;
        {ok, [PoC]} ->
            lager:info([{poc_id, POCID}], "found poc. attempting to decrypt", []),
            try 
                {ok, blockchain_ledger_poc_v2:challenger(PoC)}
            catch errormessage ->
                lager:warning([{poc_id, POCID}], "crash during getting challanger ~p", [errormessage]),
                {error, errormessage}
            end;
        {ok, _} ->
            {error, too_many_pocs}
    end.

% handle_rpc(<<"poc_find">>, Params) ->
%     ?jsonrpc_error({invalid_params, Params});

handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).