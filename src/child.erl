-module(child).

-extends(parent).

-export([module/0, a_value/0]).

module() -> ?MODULE.

a_value() -> child_value.