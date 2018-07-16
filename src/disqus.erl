-module(disqus).
-export([main/1]).

main([Input, Output]) -> parse_comments(Input, Output);
main(_) -> io:format("Usage: ~s input output~n", [escript:script_name()]).

parse_comments(Input, Output) ->
  {Element, _} = xmerl_scan:file(Input, [{space, normalize}]),
  {disqus, _, Threads} = xmerl_lib:simplify_element(Element),
  ThreadUrls =
    [{Id, Url} || {thread, [{'dsq:id', Id}], Data} <- Threads
                , {id, [], [Url]} <- Data
                ],
  Posts = [Data || {post, _, Data} <- Threads],
  Authors0 =
    lists:map(
      fun(Post) ->
          { element(2, lists:keyfind(thread, 1, Post))
          , element(3, lists:keyfind(author, 1, Post))
          }
      end, Posts),
  Authors =
    [ { proplists:get_value(ThreadId, ThreadUrls)
      , element(3, lists:keyfind(name, 1, Data))
      , element(3, lists:keyfind(email, 1, Data))
      } || {[{'dsq:id', ThreadId}], Data} <- Authors0],
  AuthorsString =
    [ io_lib:format("\"~s\", \"~s\", ~p~n", [Name, Email, Url])
    || {Url, Name, Email} <- lists:keysort(2, Authors)
    ],
  file:write_file(Output, AuthorsString).
