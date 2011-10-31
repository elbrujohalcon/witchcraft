-module(record).
-record(rec, {field1, field2}).
-export([test1/0, test2/0, test3/0]).

%% First I wrote something like this...
test1() -> [#rec{field1 = 1, field2 = 1}
            #rec{field1 = 2, field2 = 2}].
%% ...this compiles without a warning and then, when I run it...
%%    > record:test1().
%%    [#rec{field1 = 2,field2 = 2}]
%% WHAT? (o_O) Why are you ignoring my first record? (¬_¬)
%% Oooooh! I was missing a comma!!
test2() -> [#rec{field1 = 1, field2 = 1},
            #rec{field1 = 2, field2 = 2}].
%% ...now it works
%%    > record:test2().
%%    [#rec{field1 = 1,field2 = 1},#rec{field1 = 2,field2 = 2}]
%% ...but why was it compiling at all?
%% ...When I finally figured it out, I tried with the following function...
test3() -> [#rec{field1 = 1}
            #rec{field2 = 2}].
%% ...and with this one is more evident...
%%    > record:test3().
%%    [#rec{field1 = 1,field2 = 2}]
%% And with this one even more...
test4() -> [#rec{field1 = 1}#rec{field2 = 2}].