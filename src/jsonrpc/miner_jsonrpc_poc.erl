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

                lager:info("Inner Payload - ~p", [DataPacket]),

                <<IV:2/binary, OnionCompactKey:33/binary, Tag:4/binary, CipherText/binary>> = Payload,
                lager:info("Post bit manip - ~p", [DataPacket]),

                POCID = blockchain_utils:poc_id(OnionCompactKey),
                lager:info("Gotten the POCID - ~p", [DataPacket]),

                OnionKeyHash = crypto:hash(sha256, OnionCompactKey),
                lager:info("Getting Blockchain - ~p", [DataPacket]),
                BlockChain = blockchain_worker:blockchain(),
                Ledger = blockchain:ledger(BlockChain),
                lager:info("Looking it up - ~p", [DataPacket]),
                case blockchain_ledger_v1:find_pocs(OnionKeyHash, Ledger) of
                    {error, not_found} ->
                        lager:error("Not found - ~p", [DataPacket]),
                        not_found;
                    {ok, [PoC]} ->
                        lager:info([{POCID}], "found poc. attempting to decrypt", []),
                        Challenger = blockchain_ledger_poc_v2:challenger(PoC),
                        P2P = libp2p_crypto:pubkey_bin_to_p2p(Challenger),
                        #{ <<"result">> => P2P };
                    {ok, _} ->
                        lager:error("Error too many  pocs up - ~p", [DataPacket]),
                        {error, too_many_pocs}
                end;
            {error, other} ->
                lager:error("Failed proccess data packet - ~p -- ~p", [DataPacket, other]),
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

get_payload(BinaryData) ->
    case miner_lora:route(BinaryData) of
        error ->
            lager:error("Failed to deserialise the packet", []),
            {error, failure};
        {onion, Payload} ->
            lager:info("Got the payload!"),
            {ok, Payload};
        _Else ->
            lager:error("Failed to match the output of route", []),
            {error, failure}
    end.

get_onion_key({ <<_:2/binary, OnionCompactKey:33/binary>>}) ->
                                  {OnionCompactKey}.
