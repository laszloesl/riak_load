# riak_load
Simple Erlang escript to generate some load on Riak

The script takes parameters from environment variables in order
to make docker integration flexible. The following variables
can be set, displayed with the default values:

* `RIAK_HOST=localhost` - The hostname or IP of the Riak server.
* `RIAK_PORT=8087` - Riak protobuf port.
* `MAX_INSERTIONS=100` - At most this many insertions are executed in every iteration.
* `INSERTION_DELAY=10000` - The iterations are this many milliseconds apart for insertions.
* `MAX_DELETIONS=100` - At most this many deletions are executed in every iteration.
* `DELETION_DELAY=10000` - The iterations are this many milliseconds apart for deletions.

To run the script separately the following example command can be used:
```
export ERL_LIBS=${RIAK_ERLANG_CLIENT}:${RIAK_ERLANG_CLIENT}/deps/riak_pb/:${RIAK_ERLANG_CLIENT}/deps/hamcrest/
export RIAK_HOST=riak.com
export MAX_INSERTIONS=20
./riak_load.es
```
assuming that `${RIAK_ERLANG_CLIENT}` points to the cloned riak-erlang-client repo.

## Prerequisites

Obtain riak-erlang-client:
```
git clone git://github.com/basho/riak-erlang-client.git
cd riak-erlang-client
make
```

## Docker

It is more convenient to run the script in a Docker container. To build
the image manually execute:
```
docker build -t riak_test .
```
