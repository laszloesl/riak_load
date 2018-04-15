-module(riak_load_sup).
-behaviour(supervisor).

-define(FLAGS, #{strategy => one_for_one,
                 intensity => 1,
                 period => 5}).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    application:set_env(riakc, allow_listing, true),
    Procs = [
             riak_load_inserter:inserter_spec(),
             riak_load_deleter:deleter_spec()
        ],
    {ok, {?FLAGS, Procs}}.
