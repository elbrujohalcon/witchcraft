-module(edge).

-export([write_file/2]).

-spec write_file(Filename, Bytes) -> ok | {error, Reason} when
      Filename :: string(),
      Bytes :: string(),
      Reason :: file:posix() | badarg | terminated | system_limit.

write_file(Name, Bin) ->
  file:write_file(Name, Bin).
