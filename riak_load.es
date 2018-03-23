#!/usr/bin/env escript
%% -*- erlang -*-

%% Set riak client and dependencies in ERL_LIBS environment variable before run

-define(BUCKET, <<"testing">>).


%%% Main

main([]) ->
    RiakHost = os:getenv("RIAK_HOST", "localhost"),
    RiakPort = list_to_integer(os:getenv("RIAK_PORT", "8087")),
    MaxInsertions = list_to_integer(os:getenv("MAX_INSERTIONS", "100")),
    MaxDeletions = list_to_integer(os:getenv("MAX_DELETIONS", "100")),
    InsertionDelay = list_to_integer(os:getenv("INSERTION_DELAY", "10000")),
    DeletionDelay = list_to_integer(os:getenv("DELETION_DELAY", "10000")),

    application:set_env(riakc, allow_listing, true),

    spawn(
        fun () ->
            insert_random_data(RiakHost, RiakPort, InsertionDelay, MaxInsertions)
        end),
    spawn(
        fun () ->
            query_and_delete_random_data(RiakHost, RiakPort, DeletionDelay, MaxDeletions)
        end),
    receive
        _Any -> ok
    end.


%%% Connection

wait_for_riak(Host, Port) ->
    wait_for_riak(Host, Port, 15).
wait_for_riak(_Host, _Port, 0) -> error;
wait_for_riak(Host, Port, Retries) ->
    try gen_tcp:connect(Host, Port, []) of
        {ok, _} ->
            ok;
        _ ->
            timer:sleep(3000),
            wait_for_riak(Host, Port, Retries - 1)
    catch
        _:_ ->
            timer:sleep(3000),
            wait_for_riak(Host, Port, Retries - 1)
    end.

connect_to_riak(RiakHost, RiakPort) ->
    connect_to_riak(RiakHost, RiakPort, 15).
connect_to_riak(_RiakHost, _RiakPort, 0) ->
    error;
connect_to_riak(RiakHost, RiakPort, Retries) ->
    try riakc_pb_socket:start_link(RiakHost, RiakPort) of
        {ok, Pid} ->
            pong = riakc_pb_socket:ping(Pid),
            Pid;
        {error, _} ->
            timer:sleep(3000),
            connect_to_riak(RiakHost, RiakPort, Retries - 1)
    catch
        _:_ ->
            timer:sleep(3000),
            connect_to_riak(RiakHost, RiakPort, Retries - 1)
    end.


%%% Generating and putting data

insert_random_data(RiakHost, RiakPort, Delay, MaxInsertions) ->
    ok = wait_for_riak(RiakHost, RiakPort),
    Riak = connect_to_riak(RiakHost, RiakPort),
    insert_random_data(Riak, Delay, MaxInsertions).

insert_random_data(Riak, Delay, MaxInsertions) ->
    NrInsertions = rand:uniform(MaxInsertions),
    put_many(Riak, NrInsertions),
    timer:sleep(Delay),
    insert_random_data(Riak, Delay, MaxInsertions).

put_many(Riak, Count) ->
    put_to_riak(Riak, generate_data(Count)).

generate_data(Count) ->
    [riakc_obj:new(?BUCKET, undefined, N) || N <- lists:seq(1, Count)].

put_to_riak(Riak, Objs) ->
    lists:foreach(
        fun(Obj) ->
            riakc_pb_socket:put(Riak, Obj, [{dw, 1}])
        end, Objs).


%%% Reading and deleting data

query_and_delete_random_data(RiakHost, RiakPort, Delay, MaxDeletions) ->
    ok = wait_for_riak(RiakHost, RiakPort),
    Riak = connect_to_riak(RiakHost, RiakPort),
    query_and_delete_random_data(Riak, Delay, MaxDeletions).

query_and_delete_random_data(Riak, Delay, MaxDeletions) ->
    Deletions = rand:uniform(MaxDeletions),
    get_and_delete_many(Riak, Deletions),
    timer:sleep(Delay),
    query_and_delete_random_data(Riak, Delay, MaxDeletions).

get_and_delete_many(Riak, Count) ->
    lists:foreach(
        fun(_) ->
            get_and_delete_one(Riak)
        end,
        lists:seq(1, Count)),
    ok.

get_and_delete_one(Riak) ->
    case riakc_pb_socket:list_keys(Riak, ?BUCKET) of
        {ok, []} ->
            ok;
        {ok, Keys} ->
            ToDeleteIdx = rand:uniform(length(Keys)),
            ToDelete = lists:nth(ToDeleteIdx, Keys),
            riakc_pb_socket:get(Riak, ?BUCKET, ToDelete),
            riakc_pb_socket:delete(Riak, ?BUCKET, ToDelete)
    end.
