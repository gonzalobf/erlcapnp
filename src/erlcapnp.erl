-module(erlcapnp).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, NewState} = rebar3_erlcapnp_prv:init(State),
    {ok, NewState}.