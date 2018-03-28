FROM erlang:20-alpine

ADD riak_load.es /usr/bin

WORKDIR /opt
RUN apk update && \
    apk add git make && \
    wget -O /usr/bin/wait-for https://raw.githubusercontent.com/eficode/wait-for/master/wait-for && \
    chmod +x /usr/bin/wait-for && \
    git clone https://github.com/basho/riak-erlang-client.git && \
    cd riak-erlang-client && \
    make

ENV ERL_LIBS /opt/riak-erlang-client:/opt/riak-erlang-client/deps/riak_pb:/opt/riak-erlang-client/deps/hamcrest
