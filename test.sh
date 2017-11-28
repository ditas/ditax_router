#!/bin/bash

./rebar3 release

./_build/default/rel/local_release/bin/local_release start
./rebar3 ct -v --config ./config/ct.args
./_build/default/rel/local_release/bin/local_release stop