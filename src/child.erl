-module(child).

-extends(parent).

-record(?MODULE, {id :: integer(), name = "NN" :: string(), breed_id :: undefined | integer()}).

-export([module/0, a_value/0, new/0, attribute_names/0]).

module() -> ?MODULE.

a_value() -> child_value.

attribute_names() -> record_info(fields, ?MODULE).

new() -> #?MODULE{}.