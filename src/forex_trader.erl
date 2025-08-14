-module(forex_trader).
-behaviour(gen_server).

%% API
-export([start_link/0, connect/1, place_forex_trade/2, place_forex_trade/4, 
         get_order_status/1, disconnect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    ib_client = undefined,
    connected = false,
    next_order_id = undefined,
    order_status = #{}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

connect(Pid) ->
    gen_server:call(Pid, connect).

disconnect(Pid) ->
    gen_server:call(Pid, disconnect).

place_forex_trade(Pid, Action) ->
    place_forex_trade(Pid, Action, 25000, market).

place_forex_trade(Pid, Action, Quantity, OrderType) ->
    gen_server:call(Pid, {place_forex_trade, Action, Quantity, OrderType}).

get_order_status(Pid) ->
    gen_server:call(Pid, get_order_status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call(connect, _From, State) ->
    Host = application:get_env(forex_trader, ib_host, "host.docker.internal"),
    Port = application:get_env(forex_trader, ib_port, 7497),
    ClientId = application:get_env(forex_trader, client_id, 1),
    
    % Start IB client with different client ID to avoid conflicts
    case ib_client:start_link(Host, Port, ClientId + 1) of
        {ok, IBClient} ->
            case ib_client:connect(IBClient) of
                ok ->
                    io:format("Successfully connected and ready to trade!~n"),
                    timer:sleep(1000), % Wait for next_valid_id
                    NewState = State#state{ib_client = IBClient, connected = true, next_order_id = 1},
                    {reply, ok, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(disconnect, _From, State = #state{ib_client = IBClient}) ->
    case IBClient of
        undefined -> ok;
        _ -> ib_client:disconnect(IBClient)
    end,
    io:format("Disconnected from Interactive Brokers~n"),
    {reply, ok, State#state{ib_client = undefined, connected = false}};

handle_call({place_forex_trade, Action, Quantity, OrderType}, _From, 
            State = #state{ib_client = IBClient, connected = true, next_order_id = OrderId}) ->
    
    case OrderId of
        undefined ->
            io:format("No valid order ID available yet~n"),
            {reply, {error, no_order_id}, State};
        _ ->
            % Create EUR/USD contract
            Contract = create_forex_contract("EUR", "USD"),
            
            % Create order and display message
            {Order, Message} = case OrderType of
                market -> 
                    {create_market_order(Action, Quantity),
                     io_lib:format("Placing ~s market order for ~p EUR/USD~n", [Action, Quantity])};
                {limit, LimitPrice} -> 
                    {create_limit_order(Action, Quantity, LimitPrice),
                     io_lib:format("Placing ~s limit order for ~p EUR/USD @ ~p~n", [Action, Quantity, LimitPrice])}
            end,
            io:format("~s", [Message]),
            
            % PLACING REAL ORDER - Paper trading account
            io:format("🔥 PLACING REAL ORDER!~n"),
            io:format("Order ID: ~p~n", [OrderId]),
            
            % Place the actual order
            ib_client:place_order(IBClient, OrderId, Contract, Order),
            
            NewState = State#state{next_order_id = OrderId + 1},
            {reply, {ok, OrderId}, NewState}
    end;

handle_call(get_order_status, _From, State = #state{order_status = OrderStatus}) ->
    {reply, OrderStatus, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{ib_client = IBClient}) ->
    case IBClient of
        undefined -> ok;
        _ -> ib_client:disconnect(IBClient)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

create_forex_contract(BaseCurrency, QuoteCurrency) ->
    #{
        symbol => list_to_binary(BaseCurrency),
        sec_type => <<"CASH">>,
        currency => list_to_binary(QuoteCurrency),
        exchange => <<"IDEALPRO">>
    }.

create_market_order(Action, Quantity) ->
    #{
        action => list_to_binary(string:to_upper(atom_to_list(Action))),
        order_type => <<"MKT">>,
        total_quantity => Quantity
    }.

create_limit_order(Action, Quantity, LimitPrice) ->
    #{
        action => list_to_binary(string:to_upper(atom_to_list(Action))),
        order_type => <<"LMT">>,
        total_quantity => Quantity,
        lmt_price => LimitPrice
    }.