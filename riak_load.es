#!/usr/bin/env escript
%% -*- erlang -*-

%% Set riak client and dependencies in ERL_LIBS environment variable before run


%%% Main

main([]) ->
    application:ensure_started(riak_load),
    loop_forever().

loop_forever() ->
    receive
        _ -> loop_forever()
    end.
