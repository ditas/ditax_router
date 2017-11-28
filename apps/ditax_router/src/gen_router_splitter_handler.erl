%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2017 16:10
%%%-------------------------------------------------------------------
-module(gen_router_splitter_handler).
-author("pravosudov").

-callback initialize(term(), atom(), term()) -> list().
-callback start_link(term(), atom(), term()) -> {ok, pid()}.
-callback stop(pid()) -> ok.
-callback handle_data(pid(), term()) -> ok.

%% API
-export([]).
