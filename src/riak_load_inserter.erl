-module(riak_load_inserter).

-export([inserter_spec/0]).
-export([start/0]).
-export([insert_random_data/5]).

inserter_spec() ->
    #{id => inserter,
      start => {riak_load_inserter, start, []},
      restart => permanent,
      shutdown => brutal_kill}.

start() ->
    #{host := RiakHost,
      port := RiakPort,
      max_insertions := MaxInsertions,
      insertion_delay := InsertionDelay,
      bucket := Bucket} = riak_load_lib:get_riak_load_env(),
    try spawn_link(?MODULE,
                   insert_random_data,
                   [RiakHost, RiakPort, InsertionDelay, MaxInsertions, Bucket]) of
        Pid -> {ok, Pid}
    catch
        Err -> {error, Err}
    end.

insert_random_data(RiakHost, RiakPort, Delay, MaxInsertions, Bucket) ->
    Riak = riak_load_lib:connect_to_riak(RiakHost, RiakPort),
    insert_random_data(Riak, Delay, MaxInsertions, Bucket).

insert_random_data(Riak, Delay, MaxInsertions, Bucket) ->
    NrInsertions = rand:uniform(MaxInsertions),
    put_many(Riak, NrInsertions, Bucket),
    timer:sleep(Delay),
    insert_random_data(Riak, Delay, MaxInsertions, Bucket).

put_many(Riak, Count, Bucket) ->
    put_to_riak(Riak, generate_data(Count, Bucket)).

generate_data(Count, Bucket) ->
    [riakc_obj:new(Bucket, undefined, N) || N <- lists:seq(1, Count)].

put_to_riak(Riak, Objs) ->
    lists:foreach(
        fun(Obj) ->
            riakc_pb_socket:put(Riak, Obj, [{dw, 1}])
        end, Objs).
