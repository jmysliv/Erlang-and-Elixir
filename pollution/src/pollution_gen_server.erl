%%%-------------------------------------------------------------------
%%% @author jjmre
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. kwi 2020 21:58
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("jjmre").

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0, crash/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getStationWithBiggestValue/2, getStationsThatExceedsLimit/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  gen_server:start(
    {local, pollution_gen_server},
    pollution_gen_server,
    [], []).

stop() ->
  gen_server:stop(pollution_gen_server).

crash() ->
  gen_server:cast(pollution_gen_server, crash).

addStation(Name, Cords) ->
  gen_server:cast(pollution_gen_server, {addStation, Name, Cords}).

addValue(Name, Date, Type, Value) ->
  gen_server:cast(pollution_gen_server, {addValue, Name, Date, Type, Value}).

removeValue(Name, Date, Type) ->
  gen_server:cast(pollution_gen_server, {removeValue, Name, Date, Type}).

getOneValue( Name, Date, Type) ->
  gen_server:call(pollution_gen_server, {getOneValue, Name, Date, Type}).

getStationMean(Name, Type) ->
  gen_server:call(pollution_gen_server, {getStationMean,Name, Type}).

getDailyMean(Date, Type) ->
  gen_server:call(pollution_gen_server, {getDailyMean, Date, Type}).

getStationWithBiggestValue(Date, Type) ->
  gen_server:call(pollution_gen_server, {getStationWithBiggestValue,Date, Type}).

getStationsThatExceedsLimit(Date, Type, Limit) ->
  gen_server:call(pollution_gen_server, {getStationsThatExceedsLimit, Date, Type, Limit}).

%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link(
    {local, pollution_gen_server},
    pollution_gen_server
    , [], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_) ->
  {ok, pollution:createMonitor()}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({getOneValue, Name, Date, Type}, _From, Monitor) ->
  {reply, pollution:getOneValue(Name, Date, Type, Monitor), Monitor};
handle_call({getStationMean,Name, Type}, _From, Monitor) ->
  {reply, pollution:getStationMean(Name, Type, Monitor), Monitor};
handle_call({getDailyMean, Date, Type}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Type, Date, Monitor), Monitor};
handle_call({getStationWithBiggestValue, Date, Type}, _From, Monitor) ->
  {reply, pollution:getStationWithBiggestValue(Type, Date, Monitor), Monitor};
handle_call({getStationsThatExceedsLimit, Date, Type, Limit}, _From, Monitor) ->
  {reply, pollution:getStationsThatExceedsLimit(Type, Date, Limit, Monitor), Monitor}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({addStation, Name, Cords}, Monitor) ->
  {noreply, pollution:addStation(Name, Cords, Monitor)};
handle_cast({addValue, Name, Date, Type, Value}, Monitor) ->
  {noreply, pollution:addValue(Name, Date, Type, Value, Monitor)};
handle_cast({removeValue, Name, Date, Type}, Monitor) ->
  {noreply, pollution:removeValue(Name, Date, Type, Monitor)};
handle_cast(crash, _) ->
  1/0,
  {noreply, ok}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
  _Reason.

