%% @doc A supervisor that also let's you handle an associated state.
%%      Hacky acknowledgements:
%%      1. It uses the process dictionary to store the state
%%      2. It uses the fact that supervisor is implemented over gen_server
%%      3. It uses the fact that supervisor doesn't reply with hibernate or timeouts
%%      4. Module::init_state/1 function will be called before Module::init/1
%%      5. Module::terminate/1 function will be called before supervisor:terminate/1
-module(stateful_supervisor).
-extends(supervisor).
-author('elbrujohalcon@inaka.net').

-callback init(Args :: term()) ->
    {ok, {{RestartStrategy :: ?BASE_MODULE:strategy(),
           MaxR            :: non_neg_integer(),
           MaxT            :: non_neg_integer()},
           [ChildSpec :: ?BASE_MODULE:child_spec()]}}
    | ignore.
-callback init_state(Args :: term()) ->
    {ok, State :: term()} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().

-type state() :: state().

%% External exports
-export([start_link/3, start_link/4, call/2, call/3, cast/2, reply/2]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%%% ---------------------------------------------------
%%% Interface functions.
%%% ---------------------------------------------------
-spec start_link(module(), InitStateArgs::term(), InitArgs::term()) -> ?BASE_MODULE:startlink_ret().
start_link(Mod, InitStateArgs, InitArgs) ->
    gen_server:start_link(?MODULE, {self, Mod, InitStateArgs, InitArgs}, []).

-spec start_link(?BASE_MODULE:sup_name(), module(), InitStateArgs::term(), InitArgs::term()) -> ?BASE_MODULE:startlink_ret().
start_link(SupName, Mod, InitStateArgs, InitArgs) ->
    gen_server:start_link(SupName, ?MODULE, {SupName, Mod, InitStateArgs, InitArgs}, []).

-spec call(supervisor:sup_ref(), term()) -> term().
call(Name, Request) ->
    gen_server:call(Name, {?MODULE, Request}).

-spec call(supervisor:sup_ref(), term(), timeout()) -> term().
call(Name, Request, Timeout) ->
    gen_server:call(Name, {?MODULE, Request}, Timeout).

-spec cast(supervisor:sup_ref(), term()) -> ok.
cast(Name, Request) ->
    gen_server:cast(Name, {?MODULE, Request}).

-spec reply(From::term(), Reply) -> Reply.
reply(From, Reply) ->
    gen_server:reply(From, Reply).

%%% ---------------------------------------------------
%%% Internal functions.
%%% ---------------------------------------------------
-spec init({?BASE_MODULE:init_sup_name(), module(), term(), term()}) ->
        {'ok', state()} | 'ignore' | {'stop', term()}.
init({SupName, Mod, InitStateArgs, InitArgs}) ->
    try Mod:init_state(InitStateArgs) of
        {ok, InternalState} ->
            erlang:put(?MODULE, {Mod, InternalState}),
            ?BASE_MODULE:init({SupName, Mod, InitArgs});
        Other -> Other
    catch
        _:{ok, InternalState} ->
            erlang:put({?MODULE, state}, InternalState),
            ?BASE_MODULE:init({SupName, Mod, InitArgs})
    end.

-spec handle_call({?MODULE, term()} | ?BASE_MODULE:call(), term(), state()) ->
    {'reply', term(), state()} | {noreply, state()} | {stop, term(), term(), state()} | {stop, term(), state()}.
handle_call({?MODULE, Request}, From, State) ->
    {Mod, ModState} = erlang:get(?MODULE),
    try Mod:handle_call(Request, From, ModState) of
        {reply, Reply, NewState} ->
            erlang:put(?MODULE, {Mod, NewState}),
            {reply, Reply, State};
        {noreply, NewState} ->
            erlang:put(?MODULE, {Mod, NewState}),
            {noreply, State};
        {stop, Reason, Reply, NewState} ->
            erlang:put(?MODULE, {Mod, NewState}),
            {stop, Reason, Reply, State};
        {stop, Reason, NewState} ->
            erlang:put(?MODULE, {Mod, NewState}),
            {stop, Reason, State}
    catch
        _:{reply, Reply, NewState} ->
            erlang:put(?MODULE, {Mod, NewState}),
            {reply, Reply, State};
        _:{noreply, NewState} ->
            erlang:put(?MODULE, {Mod, NewState}),
            {noreply, State};
        _:{stop, Reason, Reply, NewState} ->
            erlang:put(?MODULE, {Mod, NewState}),
            {stop, Reason, Reply, State};
        _:{stop, Reason, NewState} ->
            erlang:put(?MODULE, {Mod, NewState}),
            {stop, Reason, State}
    end;
handle_call(Request, From, State) -> ?BASE_MODULE:handle_call(Request, From, State).

-spec handle_cast({?MODULE, term()} | {try_again_restart, ?BASE_MODULE:child_id() | pid()}, state()) ->
             {'noreply', state()} | {stop, term(), state()}.
handle_cast({?MODULE, Request}, State) ->
    {Mod, ModState} = erlang:get(?MODULE),
    try Mod:handle_cast(Request, ModState) of
        {noreply, NewState} ->
            erlang:put(?MODULE, NewState),
            {noreply, State};
        {stop, Reason, NewState} ->
            erlang:put(?MODULE, NewState),
            {stop, Reason, State}
    catch
        _:{noreply, NewState} ->
            erlang:put(?MODULE, NewState),
            {noreply, State};
        _:{stop, Reason, NewState} ->
            erlang:put(?MODULE, NewState),
            {stop, Reason, State}
    end;
handle_cast(Request, State) -> ?BASE_MODULE:handle_cast(Request, State).

-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
    {Mod, ModState} = erlang:get(?MODULE),
    catch Mod:terminate(ModState),
    ?BASE_MODULE:terminate(Reason, State).