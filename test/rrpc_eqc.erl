%%%-------------------------------------------------------------------
%%% @author Laura Castro <lcastro@udc.es>
%%% @copyright (C) 2012, Laura Castro
%%% @doc Assignment #7: Test state machine.
%%% @end
%%% Created : 08 Jan 2013 by Laura Castro <lcastro@udc.es>
%%%-------------------------------------------------------------------
-module(rrpc_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-compile(export_all).

-define(TEST_MODULE, rrpc).

%% Initialize the state
initial_state() ->
    [].

%% Function callbacks
% ---------------------- clear --
clear() ->
    ?TEST_MODULE:clear().

clear_args(_S) ->
    [].

clear_pre(_S) ->
    true.

clear_next(_S, _V, _Args) ->
    [].

clear_post(_S, _A, Res) ->
    Res == {ok, cleared}.

% --------------------- number --
number(Number) ->
    ?TEST_MODULE:number(Number).

number_args(_S) ->
    [int()].

number_pre(_State, [_Number]) ->
    true.

number_next(State, _V, [Number]) ->
    [Number|State].

number_post(_State, _A, R) ->
    R == {ok, saved}.

% ------------------------ add --
add() ->
    try	?TEST_MODULE:add() of
	{ok, {result, Result}} ->
	    Result;
	{error, not_allowed}->
	    not_allowed
    catch
	error:_Error ->
	    unexpected_error
    end.

add_args(_S) ->
    [].

add_pre(_S) ->
    true.

add_next([_N1,_N2|State], V, _Args) ->
    [V|State];
add_next(State, _V, _Args)->
    State.

add_post([N1,N2|_State], _A, R) ->
    R == (N1+N2);
add_post(_State, _A, R) ->
    R == not_allowed.

% -------------------------------

%% Generators

%% Frequency tunning
weight(_S, _Cmd) -> 1.

%% PROPERTY
prop_rrpc() ->
    ?FORALL(Cmds,commands(?MODULE),
	    begin
 		startup(),
		{_H, S, Res} = run_commands(?MODULE,Cmds),
		cleanup(S),
		Res == ok
		%pretty_commands(?MODULE, Cmds, {H, S, Res},
		%		Res == ok)
	    end).


%% Private utilitary stuff
startup() ->
    ok.

cleanup(_Args) ->
    ?MODULE:clear().
