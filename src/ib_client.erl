-module(ib_client).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/3, connect/1, disconnect/1, 
         req_account_summary/1, req_positions/1, place_order/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    socket = undefined,
    host = "127.0.0.1",
    port = 7497,
    client_id = 1,
    connected = false,
    next_order_id = undefined,
    account_info = #{},
    positions = []
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    Host = application:get_env(forex_trader, ib_host, "host.docker.internal"),
    Port = application:get_env(forex_trader, ib_port, 7497),
    ClientId = application:get_env(forex_trader, client_id, 1),
    start_link(Host, Port, ClientId).

start_link(Host, Port, ClientId) ->
    gen_server:start_link(?MODULE, [Host, Port, ClientId], []).

connect(Pid) ->
    gen_server:call(Pid, connect).

disconnect(Pid) ->
    gen_server:call(Pid, disconnect).

req_account_summary(Pid) ->
    gen_server:call(Pid, req_account_summary).

req_positions(Pid) ->
    gen_server:call(Pid, req_positions).

place_order(Pid, OrderId, Contract, Order) ->
    gen_server:call(Pid, {place_order, OrderId, Contract, Order}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Port, ClientId]) ->
    {ok, #state{host = Host, port = Port, client_id = ClientId}}.

handle_call(connect, _From, State = #state{host = Host, port = Port, client_id = ClientId}) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, true}]) of
        {ok, Socket} ->
            io:format("Connected to IB at ~s:~p~n", [Host, Port]),
            % Send connection handshake
            send_connect_msg(Socket, ClientId),
            NewState = State#state{socket = Socket, connected = true},
            {reply, ok, NewState};
        {error, Reason} ->
            io:format("Failed to connect: ~p~n", [Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call(disconnect, _From, State = #state{socket = Socket}) ->
    case Socket of
        undefined -> ok;
        _ -> gen_tcp:close(Socket)
    end,
    io:format("Disconnected from IB~n"),
    {reply, ok, State#state{socket = undefined, connected = false}};

handle_call(req_account_summary, _From, State = #state{socket = Socket, connected = true}) ->
    send_account_summary_req(Socket),
    {reply, ok, State};

handle_call(req_positions, _From, State = #state{socket = Socket, connected = true}) ->
    send_positions_req(Socket),
    {reply, ok, State};

handle_call({place_order, OrderId, Contract, Order}, _From, State = #state{socket = Socket, connected = true}) ->
    send_place_order(Socket, OrderId, Contract, Order),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State = #state{socket = Socket}) ->
    NewState = process_message(Data, State),
    {noreply, NewState};

handle_info({tcp_closed, Socket}, State = #state{socket = Socket}) ->
    io:format("Connection closed by IB~n"),
    {noreply, State#state{socket = undefined, connected = false}};

handle_info({tcp_error, Socket, Reason}, State = #state{socket = Socket}) ->
    io:format("TCP error: ~p~n", [Reason]),
    {noreply, State#state{socket = undefined, connected = false}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    case Socket of
        undefined -> ok;
        _ -> gen_tcp:close(Socket)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

send_connect_msg(Socket, ClientId) ->
    % IB API connection - send minimum server version and client ID
    % This matches what the Python ibapi library sends
    MinServerVersion = 38,
    MaxServerVersion = 176,
    
    % Send connection string
    ConnectStr = io_lib:format("~p\0~p\0", [MinServerVersion, ClientId]),
    Msg = iolist_to_binary(ConnectStr),
    
    io:format("Sending connection message: ~p~n", [Msg]),
    gen_tcp:send(Socket, Msg).

send_account_summary_req(Socket) ->
    % Simplified account summary request
    ReqId = 1,
    Group = <<"All">>,
    Tags = <<"NetLiquidation,TotalCashValue,BuyingPower">>,
    Msg = <<"62\0", (integer_to_binary(ReqId))/binary, "\0", 
            Group/binary, "\0", Tags/binary, "\0">>,
    gen_tcp:send(Socket, Msg).

send_positions_req(Socket) ->
    % Request positions
    Msg = <<"61\0">>,
    gen_tcp:send(Socket, Msg).

send_place_order(Socket, OrderId, Contract, Order) ->
    % IB API Place Order message (message ID 3)
    #{symbol := Symbol, sec_type := SecType, currency := Currency, exchange := Exchange} = Contract,
    #{action := Action, order_type := OrderType, total_quantity := Quantity} = Order,
    
    % Build proper IB API place order message
    Fields = [
        <<"3">>,                                    % Message ID for place order
        integer_to_binary(OrderId),                 % Order ID
        <<"0">>,                                    % Contract ID (0 for new)
        Symbol,                                     % Symbol
        SecType,                                    % Security type
        <<"">>,                                     % Expiry (empty for cash)
        <<"0">>,                                    % Strike (0 for cash)
        <<"">>,                                     % Right (empty for cash)
        <<"1">>,                                    % Multiplier (1 for cash)
        Exchange,                                   % Exchange
        <<"">>,                                     % Primary exchange (empty)
        Currency,                                   % Currency
        <<"">>,                                     % Local symbol (empty)
        <<"">>,                                     % Trading class (empty)
        <<"0">>,                                    % Include expired (0)
        <<"">>,                                     % Sec ID type (empty)
        <<"">>,                                     % Sec ID (empty)
        Action,                                     % Action (BUY/SELL)
        integer_to_binary(Quantity),                % Total quantity
        OrderType,                                  % Order type (MKT/LMT)
        case OrderType of
            <<"LMT">> -> 
                LmtPrice = maps:get(lmt_price, Order, 0),
                float_to_binary(LmtPrice, [{decimals, 4}]);
            _ -> <<"0">>
        end,                                        % Limit price
        <<"0">>,                                    % Aux price
        <<"GTC">>,                                  % Time in force
        <<"">>,                                     % OCA group
        <<"">>,                                     % Account
        <<"0">>,                                    % Open/close
        <<"0">>,                                    % Origin
        <<"0">>,                                    % Order ref
        <<"1">>,                                    % Transmit
        <<"0">>,                                    % Parent ID
        <<"0">>,                                    % Block order
        <<"0">>,                                    % Sweep to fill
        <<"0">>,                                    % Display size
        <<"0">>,                                    % Trigger method
        <<"0">>,                                    % Outside RTH
        <<"0">>                                     % Hidden
    ],
    
    % Join fields with null terminators
    Msg = iolist_to_binary([lists:join(<<"\0">>, Fields), <<"\0">>]),
    io:format("Sending place order message: ~p bytes~n", [byte_size(Msg)]),
    gen_tcp:send(Socket, Msg).

process_message(Data, State) ->
    % Parse incoming messages from IB
    case parse_ib_message(Data) of
        {connect_ack} ->
            io:format("Connection acknowledged~n"),
            State;
        {next_valid_id, OrderId} ->
            io:format("Next valid order ID: ~p~n", [OrderId]),
            State#state{next_order_id = OrderId};
        {managed_accounts, Accounts} ->
            io:format("Managed accounts: ~s~n", [Accounts]),
            State;
        {account_summary, Account, Tag, Value, Currency} ->
            io:format("Account: ~s, ~s: ~s ~s~n", [Account, Tag, Value, Currency]),
            AccountInfo = maps:put({Account, Tag}, {Value, Currency}, State#state.account_info),
            State#state{account_info = AccountInfo};
        {position, Account, Symbol, Position, AvgCost} ->
            io:format("Position: ~s - ~p @ ~p~n", [Symbol, Position, AvgCost]),
            PosInfo = #{account => Account, symbol => Symbol, position => Position, avg_cost => AvgCost},
            Positions = [PosInfo | State#state.positions],
            State#state{positions = Positions};
        {order_status, OrderId, Status, Filled, Remaining, AvgFillPrice} ->
            io:format("Order ~p: Status=~s, Filled=~p, Remaining=~p, AvgPrice=~p~n", 
                     [OrderId, Status, Filled, Remaining, AvgFillPrice]),
            State;
        {execution, Symbol, Side, Shares, Price} ->
            io:format("Execution: ~s ~s ~p @ ~p~n", [Symbol, Side, Shares, Price]),
            State;
        {error, ErrorCode, ErrorString} ->
            io:format("Error ~p: ~s~n", [ErrorCode, ErrorString]),
            State;
        unknown ->
            State
    end.

parse_ib_message(Data) ->
    % Parse IB API messages
    try
        case binary:split(Data, <<"\0">>, [global]) of
            [<<"1">> | _] -> 
                {connect_ack};
            [<<"9">>, OrderIdBin | _] -> 
                OrderId = binary_to_integer(OrderIdBin),
                {next_valid_id, OrderId};
            [<<"15">>, Accounts | _] ->
                {managed_accounts, Accounts};
            [<<"3">>, OrderIdBin, Status, Filled, Remaining, AvgFillPrice | _] ->
                OrderId = binary_to_integer(OrderIdBin),
                FilledQty = case Filled of
                    <<>> -> 0;
                    _ -> binary_to_integer(Filled)
                end,
                RemainingQty = case Remaining of
                    <<>> -> 0;
                    _ -> binary_to_integer(Remaining)
                end,
                AvgPrice = case AvgFillPrice of
                    <<>> -> 0.0;
                    _ -> binary_to_float(AvgFillPrice)
                end,
                {order_status, OrderId, Status, FilledQty, RemainingQty, AvgPrice};
            [<<"4">>, ErrorCode, ErrorMsg | _] ->
                Code = case ErrorCode of
                    <<>> -> 0;
                    _ -> binary_to_integer(ErrorCode)
                end,
                {error, Code, ErrorMsg};
            _ -> 
                io:format("Unknown message: ~p~n", [Data]),
                unknown
        end
    catch
        _:_ -> 
            io:format("Failed to parse message: ~p~n", [Data]),
            unknown
    end.