-module(forex_cli).
-export([main/1, test_connection/0, safe_trader/0, live_trader/0]).

%%====================================================================
%% Main CLI interface
%%====================================================================

main([]) ->
    print_usage();

main([test]) ->
    test_connection();

main([safe]) ->
    safe_trader();

main([live]) ->
    live_trader();

main(["test"]) ->
    test_connection();

main(["safe"]) ->
    safe_trader();

main(["live"]) ->
    live_trader();

main(_) ->
    print_usage().

%%====================================================================
%% Commands
%%====================================================================

test_connection() ->
    io:format("Starting Interactive Brokers API client...~n"),
    
    case ib_client:start_link() of
        {ok, Client} ->
            case ib_client:connect(Client) of
                ok ->
                    io:format("Successfully connected to Interactive Brokers!~n"),
                    
                    % Just test the connection, don't try to send complex messages yet
                    io:format("Connection test successful - TCP socket established~n"),
                    
                    % Wait a moment
                    timer:sleep(2000),
                    
                    io:format("~n==================================================~n"),
                    io:format("CONNECTION SUMMARY~n"),
                    io:format("==================================================~n"),
                    io:format("Connected: true~n"),
                    io:format("Host: host.docker.internal~n"),
                    io:format("Port: 7497~n"),
                    io:format("Test completed successfully~n"),
                    
                    ib_client:disconnect(Client),
                    io:format("~nDisconnected from Interactive Brokers~n");
                {error, Reason} ->
                    io:format("Failed to connect: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("Failed to start client: ~p~n", [Reason])
    end.

safe_trader() ->
    io:format("Starting EUR/USD Forex Trader...~n"),
    
    case forex_trader:start_link() of
        {ok, Trader} ->
            case forex_trader:connect(Trader) of
                ok ->
                    io:format("~n==================================================~n"),
                    io:format("PLACING FOREX TRADE~n"),
                    io:format("==================================================~n"),
                    
                    % Place a test trade (won't actually execute)
                    case forex_trader:place_forex_trade(Trader, buy, 25000, market) of
                        {ok, OrderId} ->
                            io:format("Trade setup completed with Order ID: ~p~n", [OrderId]);
                        {error, Reason} ->
                            io:format("Failed to setup trade: ~p~n", [Reason])
                    end,
                    
                    % Wait a bit
                    timer:sleep(3000),
                    
                    % Show order status
                    OrderStatus = forex_trader:get_order_status(Trader),
                    case maps:size(OrderStatus) of
                        0 -> io:format("~nNo orders placed (safe mode)~n");
                        _ -> 
                            io:format("~nOrder Status Summary:~n"),
                            maps:fold(fun(OrderId, Status, _) ->
                                io:format("Order ~p: ~p~n", [OrderId, Status])
                            end, ok, OrderStatus)
                    end,
                    
                    forex_trader:disconnect(Trader);
                {error, Reason} ->
                    io:format("Failed to connect: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("Failed to start trader: ~p~n", [Reason])
    end.

live_trader() ->
    io:format("⚠️  WARNING: This will attempt to place a REAL trade!~n"),
    io:format("Press Ctrl+C to cancel, or wait 3 seconds to continue...~n"),
    timer:sleep(3000),
    
    case forex_trader_live:run_live_trade(buy, 1000) of
        {ok, Result} ->
            io:format("Live trade completed: ~p~n", [Result]);
        {error, Reason} ->
            io:format("Live trade failed: ~p~n", [Reason])
    end.

%%====================================================================
%% Helper functions
%%====================================================================

print_usage() ->
    io:format("Interactive Brokers Forex Trading System~n"),
    io:format("~nUsage:~n"),
    io:format("  erl -pa ebin -s forex_cli main test    - Test IB connection~n"),
    io:format("  erl -pa ebin -s forex_cli main safe    - Safe trader (no real trades)~n"),
    io:format("  erl -pa ebin -s forex_cli main live    - Live trader (REAL trades!)~n"),
    io:format("~nOr from Erlang shell:~n"),
    io:format("  forex_cli:test_connection().~n"),
    io:format("  forex_cli:safe_trader().~n"),
    io:format("  forex_cli:live_trader().~n").