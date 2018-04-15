PROJECT = riak_load
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

# Whitespace to be used when creating files from templates.
SP = 4

include erlang.mk

docker:
	docker build -t riak_load:latest .
