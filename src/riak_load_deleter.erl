-module(riak_load_deleter).

-export([deleter_spec/0]).
-export([start/0]).
-export([query_and_delete_random_data/5]).

deleter_spec() ->
    #{id => deleter,
      start => {riak_load_deleter, start, []},
      restart => permanent,
      shutdown => brutal_kill}.

start() ->
    #{host := RiakHost,
      port := RiakPort,
      max_deletions := MaxDeletions,
      deletion_delay := DeletionDelay,
      bucket := Bucket} = riak_load_lib:get_riak_load_env(),
    try spawn_link(?MODULE,
                   query_and_delete_random_data,
                   [RiakHost, RiakPort, DeletionDelay, MaxDeletions, Bucket]) of
        Pid -> {ok, Pid}
    catch
        Err -> {error, Err}
    end.

query_and_delete_random_data(RiakHost, RiakPort, Delay, MaxDeletions, Bucket) ->
    Riak = riak_load_lib:connect_to_riak(RiakHost, RiakPort),
    query_and_delete_random_data(Riak, Delay, MaxDeletions, Bucket).

query_and_delete_random_data(Riak, Delay, MaxDeletions, Bucket) ->
    Deletions = rand:uniform(MaxDeletions),
    timer:sleep(Delay),
    get_and_delete_many(Riak, Deletions, Bucket),
    query_and_delete_random_data(Riak, Delay, MaxDeletions, Bucket).

get_and_delete_many(Riak, Count, Bucket) ->
    lists:foreach(
        fun(_) ->
            get_and_delete_one(Riak, Bucket)
        end,
        lists:seq(1, Count)),
    ok.

get_and_delete_one(Riak, Bucket) ->
    case riakc_pb_socket:list_keys(Riak, Bucket) of
        {ok, []} ->
            ok;
        {ok, Keys} ->
            ToDeleteIdx = rand:uniform(length(Keys)),
            ToDelete = lists:nth(ToDeleteIdx, Keys),
            riakc_pb_socket:get(Riak, Bucket, ToDelete),
            riakc_pb_socket:delete(Riak, Bucket, ToDelete)
    end.
