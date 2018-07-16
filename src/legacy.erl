-module legacy.

%%% GENERAL CONSIDERATIONS
% > In dynamically typed languages, where we don't really have classes, it's not enough to fake
%   the parameters to functions in modules or build fake objects for things returned by certain
%   functions (i.e. when you have V = this:thing(...), it's not enough to be able to mock 'this'
%   or 'thing' or provide fake parameters to it, you also need to figure out what 'this:thing'
%   may evaluate to, which is not trivially determined by "the type of V" since V has no type).


%%% ADAPT PARAMETER
-spec adapt_parameter(http_servlet_request:t()) -> _.
adapt_parameter(HttpRequest) ->
    Values = http_servlet_request:parameter_values(HttpRequest),
    do:some_stuff(with, Values).

somewhere_else(HttpRequest) ->
    adapt_parameter(HttpRequest).

-spec new_adapt_parameter(parameter_source:t()) -> _.
new_adapt_parameter(ParamSource) ->
    Values = parameter_source:values(HttpRequest),
    do:some_stuff(with, Values).

new_somewhere_else(HttpRequest) ->
    % http_request_param_source implements behavior parameter_source
    new_adapt_parameter(http_request_param_source:new(HttpRequest)).


%%% BREAK OUT METHOD OBJECT
a_long_method(With, Many, Parameters) ->
    does:stuff(),
    then:calls(),
    a_private_function(With, Parameters),
    then:more(stuff).

new_a_log_method(With, Many, Parameters) ->
    new_module:new_a_long_method(With, Many, Parameters).

%% In a new module...
new_a_long_method(With, Many, Parameters) ->
    does:stuff(),
    then:calls(),
    legacy:a_private_function(With, Parameters),
    %% We must export a_private_function now
    then:more(stuff).


%%% DEFINITION COMPLETION doesn't apply to Erlang
% > Redefine methods for tests (since they're specified in .h files)


%%% ENCAPSULATE GLOBAL REFERENCES doesn't _directly_ apply to Erlang
% > Make global variables internal to a class with a singleton


%%% EXPOSE STATIC METHOD
% > Instead of "making" the method static (which all are in Erlang), we just move it away from
%     the original module to some _utils_ module.
-spec a_function_in_a_huge_module(that, doesnt, use, t()) -> _.
%% just move it to its own module along with any associated private functions
a_function_in_a_huge_module(That, Doesnt, Use, T) ->
    new_module:a_function_in_a_huge_module(That, Doesnt, Use, T).


%%% EXTRACT AND OVERRIDE CALL
a_function(With) ->
    Stuff = that:calls(another, function, in),
    A = strange:module(With, Stuff),
    then:does(something, with, A).

new_a_function(With) ->
    Stuff = that:calls(another, function, in),
    A = strange_module(With, Stuff),
    then:does(something, with, A).

%% This one can be mocked without mocking new_a_function nor strange:module/2
strange_module(With, Stuff) ->
    strange:module(With, Stuff).


%%% EXTRACT AND OVERRIDE FACTORY METHOD
another_long_function(With, Parameters) ->
    Thing1 = thing1s:new(Parameters),
    Thing2 = thing2s:new(Thing1, Parameters),
    WhatsActuallyUsed = actually_used_things:new(Thing1, Thing2),
    a_lot:of_stuff(With, WhatsActuallyUsed).

new_another_long_function(With, Parameters) ->
    WhatsActuallyUsed = build_what_is_actually_used(Parameters),
    a_lot:of_stuff(With, WhatsActuallyUsed).

%% This can be mocked independently of new_another_long_function/2, thing1s, thing2s, etc...
build_what_is_actually_used(Parameters) ->
    Thing1 = thing1s:new(Parameters),
    Thing2 = thing2s:new(Thing1, Parameters),
    actually_used_things:new(Thing1, Thing2).


%%% EXTRACT AND OVERRIDE GETTER
new() ->
    #something{
        of_type = t(),
        that_includes = something:that_is(hard, to, fake),
        and_many = other_things()
    }.

that_includes(#something{that_includes = That}) -> That.

new_new() ->
    #something{
        of_type => t(),
        that_includes => undefined,
        and_many => other_things()
    }.

%% This is a lazy getter and has to be used everywhere instead of picking the field out of the
%% record. Then, it can be mocked without replacing the constructor or faking something:that_is/3
that_includes(#something{that_includes = undefined} = T) ->
    that_includes(T#something{that_includes = something:that_is(hard, to, fake)});
that_includes(#something{that_includes = That}, T) -> {That, T}.

% > I don't like this technique at all. There must be better ways of doing the same.


%%% EXTRACT IMPLEMENTER doesn't _directly_ apply to Erlang
% > Turn a class into an interface and move the actual implementation to a subclass of it


%%% EXTRACT INTERFACE doesn't _directly_ apply to Erlang
% > Extract an interface from a concrete class, then create a mock for it and use it in tests
% > In dynamic typed languages there is no need to create an interface in order to build mocks


%%% INTRODUCE INSTANCE DELEGATOR
% > This is a super complex way of mocking stuff, but for completeness sake...
in_this_function() ->
    You = want:to(mock, calls),
    To = want:module(but, You),
    Dont = want:to(use, meck).

new_in_this_function(Want) ->
    You = Want:to(mock, calls),
    To = Want:module(but, You),
    Dont = Want:to(use, meck).
% > You provide the module as a parameter, so that you can provide a fake module in the tests.
%   Eventually, the module with in_this_function/0 may become a behavior.


%%% INTRODUCE STATIC SETTER
% > Not really applicable since none of our variables are global.
%   The closest thing I can think of is a private ets table held by a process that doesn't provide
%   a way to write data to it.
%   In this case, you can follow this Introduce a Static Setter method and add a way to write the
%   data, just for tests. Or if the process is behind a functional interface, just mock its
%   functions. You can even mock the process by unregistering it, spawning another one, registering
%   this new one with the name of the old one and letting it impersonate the other.


%%% LINK SUBSTITUTION doesn't apply to Erlang
% > It's for procedural languages, where you replace stuff through the linker


%%% PARAMETERIZE CONSTRUCTOR
-spec new(pos_integer(), binary()) -> t().
new(Param1, Param2) ->
    #{
        thing1 = thing1s:new(Param1),
        thing2 = thing2s:new(Param2)
    }.

-spec new_new(thing1s:t(), thing2s:t()) -> t().
new_new(Thing1, Thing2) ->
    #{
        thing1 = Thing1,
        thing2 = Thing2
    }.

% can keep the original new/2, too
-spec new(pos_integer(), binary()) -> t().
new(Param1, Param2) -> new_new(thing1s:new(Param1), thing2s:new(Param2)).


%%% PARAMETERIZE METHOD
function() ->
    Thing = thing1s:new(),
    do:stuff(with, Thing).

new_function(Thing) ->
    do:stuff(with, Thing).

% We can keep the original function/0, too
function() -> function(thing1s:new()).


%%% PRIMITIVIZE PARAMETER
-spec nested_computation(t()) -> integer().
nested_computation(#{level1things = L1Things}) ->
    Values = [l1things:nested_computation(T) || T <- L1Things],
    % These are not calls to other modules, this is code inlined here
    Result = do:a_bunch(of, things, on, these, Values),
    Result.

-spec new_nested_computation(t()) -> integer().
new_nested_computation(#{level1things = L1Things}) ->
    Values = [l1things:nested_computation(T) || T <- L1Things],
    %% Module call doesn't know about the complex things, just processes integers, binaries, stuff
    Result = call:another_module_with_just(Values),
    Result.

%%% PULL UP FEATURE doesn't _directly_ apply to Erlang
% > move a bunch of methods in the class you want to test up to an abstract superclass of that one
%   with all other methods declared as abstract ones, make the original class a subclass of it
%   create a new subclass for testing so that you don't need to care about the _other_ methods.


%%% PUSH DOWN DEPENDENCY doesn't _directly_ apply to Erlang
% > very similar to the previous one, but you keep the original class and you make it abstract
%   then push the code and dependencies you have issues with to a new subclass, keeping the methods
%   you want to test in the original one.


%%% REPLACE FUNCTION WITH FUNCTION POINTER doesn't apply to Erlang
% > It's for procedural languages, where you replace stuff through the linker


%%% REPLACE GLOBAL REFERENCE WITH GETTER
% > Similar to INTRODUCE STATIC SETTER
%   Basically if you have some info that's in a process private state and exposed only through
%   message passing to the process, just build a function that encapsulates the message passing
%   and use it everywhere (for instance, instead of just using gen_server:call/2,3). That way
%   you can mock that particular function later.


%%% SUBCLASS AND OVERRIDE METHOD
% > This is the core technique for replacing behavior in an object.
%   In dynamic languages like Erlang that's achieved by basically mocking functions.
%   In the same way that in OOP that may require making private methods protected, in Erlang it may
%   require making private functions exported.


%%% SUPERSEDE INSTANCE VARIABLE
-spec new(param:t()) -> t().
new(Param) ->
    #{  % There is no way to change the value of `thing` from the outside of this ODS
        thing => built_with(Param)
    }.

% @hidden
% @doc Test method to change the `thing` for tests
-spec supersede_test(t(), things:t()) -> t().
supersede_test(T, Thing) -> T#{thing := Thing}.

% > We basically added a setter for something that should *not* be set in a real production system
%   that's why we use the supersede prefix to be able to grep the rest of the code and verify that
%   this function is actually not called in production code. This can be verified by elvis, too.


%%% TEMPLATE REDEFINITION
-spec new() -> t().
new() ->
    #{socket => gen_tcp:connect("localhost", 3311, [])}.

-spec send_message(t(), iodata()) -> _.
send_message(#{socket := Socket}, Msg) ->
    gen_tcp:send(Socket, Msg).

% > Similar to ADAPT PARAMETER, but taken a step further. Turn your module into a behavior and
%   delegate the calls that you want to avoid to other modules. Your original code goes into the
%   default implementation of the new behavior and the tests can use a fake one.

-type callback_state() :: term().
-callback init(string(), pos_integer()) -> callback_state().
-callback send(callback_state(), iodata()) -> ok.

-spec new_new(module()) -> t().
new_new(Module) ->
    #{mod => Module, state => Module:init("localhost", 3311)}.

-spec send_message(t(), iodata()) -> _.
send_message(#{mod := Mod, state := State}, Msg) ->
    Mod:send(State, Msg).


%%% TEXT REDEFINITION
% > Basically monkey patching
