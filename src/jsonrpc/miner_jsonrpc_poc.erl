-module(miner_jsonrpc_poc).

-include("miner_jsonrpc.hrl").
-behavior(miner_jsonrpc_handler).
-export([handle_rpc/2]).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"poc_find">>, #{ <<"key">> := DataPacket }) ->
    try
        lager:info("Request received to get PoC - ~p", [DataPacket]),

        BinaryData = base64:decode(DataPacket),
        lager:info("Packet converted to binary"),
    
        case get_payload(BinaryData) of 

            {ok, Payload} ->

                <<IV:2/binary, OnionCompactKey:33/binary, Tag:4/binary, CipherText/binary>> = Data,

                POCID = blockchain_utils:poc_id(OnionCompactKey),
                OnionKeyHash = crypto:hash(sha256, OnionCompactKey),
                lager:info("Getting Blockchain - ~p", [DataPacket]),
                Ledger = blockchain:ledger(blockchain:blockchain()),
                lager:info("Looking it up - ~p", [DataPacket]),
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
                lager:error("Failed proccess data packet - ~p", [DataPacket]),
                ?jsonrpc_error({failed_proccess_packet, DataPacket})
        end
    catch
        a:b ->
            lager:error("Failed to get PoC - ~p  -  ~p", [a, b]),
            ?jsonrpc_error({invalid_params, DataPacket})
    end;


handle_rpc(<<"poc_find">>, Params) ->
    ?jsonrpc_error({invalid_params, Params});

handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_payload(binary()) -> any().
get_payload(BinaryData) ->
    case miner_lora:route(BinaryData) of
        error ->
            lager:error("Failed to deserialise the packet", []),
            {error, failure};
        {onion, Payload} ->
            lager:info("Got the payload!"),
            {ok, Payload}
    end,
    {error, failure}.

get_onion_key({ <<_:2/binary, OnionCompactKey:33/binary>>}) ->
                                  {OnionCompactKey}.
