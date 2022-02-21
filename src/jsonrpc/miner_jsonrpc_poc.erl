-module(miner_jsonrpc_poc).

-include("miner_jsonrpc.hrl").
-behavior(miner_jsonrpc_handler).
-export([handle_rpc/2]).

%%
%% jsonrpc_handler
%%


handle_rpc(<<"poc_find">>, #{ <<"key">> := DataPacket }) ->
    try
        lager:info([{poc_id}], "Request received to get PoC - ~p", [DataPacket]),

        case get_payload(DataPacket) of 

            {ok, Payload} ->
                BinKey = get_onion_key(BinMessage),                            
                POCID = blockchain_utils:poc_id(BinKey),
                OnionKeyHash = crypto:hash(sha256, BinKey),
                lager:info([{poc_id}], "Getting Blockchain - ~p", [DataPacket]),
                Ledger = blockchain:ledger(blockchain:blockchain()),
                lager:info([{poc_id}], "Looking it up - ~p", [DataPacket]),
                case blockchain_ledger_v1:find_pocs(OnionKeyHash, Ledger) of
                    {error, not_found} ->
                        not_found;
                    {ok, [PoC]} ->
                        lager:info([{poc_id, POCID}], "found poc. attempting to decrypt", []),
                        Challanger = blockchain_ledger_poc_v2:challenger(PoC),
                        {ok, Challanger};
                    {ok, _} ->
                        {error, too_many_pocs}
                end;
            {error, _} ->
                lager:error([{poc_id}], "Failed proccess data packet - ~p", [DataMessage]),
                ?jsonrpc_error({failed_proccess_packet, DataMessage})
        end;
    catch
        _:_ ->
            lager:error([{poc_id}], "Failed to get PoC - ~p", [DataMessage]),
            ?jsonrpc_error({invalid_params, DataMessage})
    end;


handle_rpc(<<"poc_find">>, Params) ->
    ?jsonrpc_error({invalid_params, Params});

handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).


get_payload(DataPacket) ->
    case miner_lora:route(DataPacket) of
        error ->
            {error, failure};
        {onion, Payload} ->
            {ok, Payload};
        {noop, non_longfi} ->
            lager:debug("Unable to get PoC on non Lofi packet ~p", [DataPacket])
    end,
    {error, failure}.
    
get_onion_key({ <<_:2/binary, OnionCompactKey:33/binary>>}) ->
                                  {OnionCompactKey}.
