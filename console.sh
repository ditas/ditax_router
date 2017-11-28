#!/bin/bash

./rebar3 release

./_build/default/rel/local_release/bin/local_release console

