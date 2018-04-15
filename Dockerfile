FROM erlang:20-alpine

COPY erlang.mk Makefile /opt/riak_load/
COPY src /opt/riak_load/src

ADD riak_load.es /usr/bin

RUN apk update && \
    apk add git make && \
    cd /opt/riak_load && \
    make && \
    wget -O /usr/bin/wait-for https://raw.githubusercontent.com/eficode/wait-for/master/wait-for && \
    chmod +x /usr/bin/wait-for && \
    cd /opt && \
    git clone https://github.com/basho/riak-erlang-client.git /opt/riak-erlang-client && \
    cd /opt/riak-erlang-client && \
    make

ENV ERL_LIBS /opt/riak-erlang-client:/opt/riak-erlang-client/deps/riak_pb:/opt/riak-erlang-client/deps/hamcrest:/opt/riak_load
