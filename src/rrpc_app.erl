%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2013, Laura M. Castro
%%% @doc
%%%
%%% @end
%%% Created : 17 Jan 2013
%%%-------------------------------------------------------------------
-module(rrpc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
     rrpc_sup:start_link().

stop(_State) ->
    ok.
