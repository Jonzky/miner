-module(miner_jsonrpc_poc).

-include("miner_jsonrpc.hrl").
-behavior(miner_jsonrpc_handler).
-export([handle_rpc/2]).

%%
%% jsonrpc_handler
%%


handle_rpc(<<"poc_find">>, #{ <<"key">> := DataMessage }) ->
    try
        lager:info([{poc_id}], "Request received to get PoC - ~p", [DataMessage]),
        BinMessage = ?B64_TO_BIN(DataMessage),
        BinKey = get_onion_key(BinMessage),                            
        POCID = blockchain_utils:poc_id(BinKey),
        OnionKeyHash = crypto:hash(sha256, BinKey),
        lager:info([{poc_id}], "Getting Blockchain - ~p", [DataMessage]),
        Ledger = blockchain:ledger(blockchain:blockchain()),
        lager:info([{poc_id}], "Looking it up - ~p", [DataMessage]),
        case blockchain_ledger_v1:find_pocs(OnionKeyHash, Ledger) of
            {error, not_found} ->
                not_found;
            {ok, [PoC]} ->
                lager:info([{poc_id, POCID}], "found poc. attempting to decrypt", []),
                Challanger = blockchain_ledger_poc_v2:challenger(PoC),
                {ok, Challanger};
            {ok, _} ->
                {error, too_many_pocs}
        end
    catch
        _:_ ->
            lager:info([{poc_id}], "Failed to get PoC - ~p", [DataMessage]),
            ?jsonrpc_error({invalid_params, DataMessage})
    end;


handle_rpc(<<"poc_find">>, Params) ->
    ?jsonrpc_error({invalid_params, Params});

handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

get_onion_key({ <<_:2/binary, OnionCompactKey:33/binary>>}) ->
                                  {OnionCompactKey}.
