FROM erlang:latest
ADD riak_load.es /usr/bin

WORKDIR /opt
RUN git clone git://github.com/basho/riak-erlang-client.git && \
    cd riak-erlang-client && \
    make

ENV ERL_LIBS /opt/riak-erlang-client:/opt/riak-erlang-client/deps/riak_pb:/opt/riak-erlang-client/deps/hamcrest
