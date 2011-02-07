-module(atom_queue, [Queue]).

-export([push/1, pop/0]).

-spec pop() -> {undefined | atom(), {?MODULE, [atom()]}}.
pop() -> case Queue of
           [] -> {undefined, atom_queue:new([])};
           [X|Rest] -> {X, atom_queue:new(Rest)}
         end.

-spec push(atom()) -> {?MODULE, [atom()]}.
push(X) -> atom_queue:new(lists:reverse([X|Queue])).