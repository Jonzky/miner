-module(miner_profile).

-compile(export_all).


requests() ->
    Chain = blockchain_worker:blockchain(),
    Ledger = blockchain:ledger(Chain),
    {ok, Height} = blockchain_ledger_v1:current_height(Ledger),
    requests(Height).

requests(Height) ->
    Chain = blockchain_worker:blockchain(),
    {ok, Block} = blockchain:get_block(Height, Chain),
    Reqs = filter_requests(Block),
    profile_txns(Reqs, Height, Chain).

filter_requests(Block) ->
    lists:filter(
      fun(T) ->
              blockchain_txn:type(T) == blockchain_txn_poc_request_v1
      end, blockchain_block:transactions(Block)).

filter_receipts(Block) ->
    lists:filter(
      fun(T) ->
              blockchain_txn:type(T) == blockchain_txn_poc_receipts_v1
      end, blockchain_block:transactions(Block)).

profile_txns(Reqs, Height, Chain) ->
    {ok, LedgerAt0} = blockchain:ledger_at(Height - 1, Chain),
    LedgerAt = blockchain_ledger_v1:new_context(LedgerAt0),
    Chain1 = blockchain:ledger(LedgerAt, Chain),
    eprof:start_profiling([self()]),
    blockchain_txn:validate(Reqs, Chain1),
    eprof:stop_profiling(),
    blockchain_ledger_v1:delete_context(LedgerAt),
    ok.

receipts() ->
    Chain = blockchain_worker:blockchain(),
    Ledger = blockchain:ledger(Chain),
    {ok, Height} = blockchain_ledger_v1:current_height(Ledger),
    receipts(Height).

receipts(Height) ->
    Chain = blockchain_worker:blockchain(),
    {ok, Block} = blockchain:get_block(Height, Chain),
    Reqs = filter_requests(Block),
    Recs = filter_receipts(Block),
    profile_receipts(Reqs, Recs, Height, Chain).

profile_receipts(Reqs, Recs, Height, Chain) ->
    {ok, LedgerAt0} = blockchain:ledger_at(Height - 1, Chain),
    LedgerAt = blockchain_ledger_v1:new_context(LedgerAt0),
    Chain1 = blockchain:ledger(LedgerAt, Chain),
    lists:foreach(fun(T) -> blockchain_txn:absorb(T, Chain1) end, Reqs),
    eprof:start_profiling([self()]),
    blockchain_txn:validate(Recs, Chain1),
    eprof:stop_profiling(),
    blockchain_ledger_v1:delete_context(LedgerAt),
    ok.