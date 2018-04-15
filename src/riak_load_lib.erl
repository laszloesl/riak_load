-module(riak_load_lib).

-export([connect_to_riak/2,
         get_riak_load_env/0]).

connect_to_riak(RiakHost, RiakPort) ->
    connect_to_riak(RiakHost, RiakPort, 15).

connect_to_riak(_RiakHost, _RiakPort, 0) ->
    {error, "Could not connect to Riak."};
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

get_riak_load_env() ->
    #{host => os:getenv("RIAK_HOST", "localhost"),
      bucket => list_to_binary(os:getenv("RIAK_BUCKET", "testing")),
      port => list_to_integer(os:getenv("RIAK_PORT", "8087")),
      max_insertions => list_to_integer(os:getenv("MAX_INSERTIONS", "100")),
      max_deletions => list_to_integer(os:getenv("MAX_DELETIONS", "100")),
      insertion_delay => list_to_integer(os:getenv("INSERTION_DELAY", "10000")),
      deletion_delay => list_to_integer(os:getenv("DELETION_DELAY", "10000"))}.
