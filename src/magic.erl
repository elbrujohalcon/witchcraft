%% @doc A module used to test some erlang magic for inaka's blog
-module(magic).

-export([whats_in/1, say/1]).

-spec whats_in(term()) -> term().
whats_in(this_hat) ->
	case get(magic) of
		undefined -> nothing;
		Something -> Something
	end.

-spec say(term()) -> ok.
say(abracadabra) ->
	_ = put(magic, 'a rabbit'),
	ok.
