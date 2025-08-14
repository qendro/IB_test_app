-module(ib_test).
-export([test_raw_connection/0]).

test_raw_connection() ->
    Host = "host.docker.internal",
    Port = 7497,
    
    io:format("Testing raw TCP connection to ~s:~p~n", [Host, Port]),
    
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}], 5000) of
        {ok, Socket} ->
            io:format("✅ TCP connection successful!~n"),
            
            % Send IB API connection message (like Python ibapi)
            TestMsg = <<"38\0", "1\0">>,
            case gen_tcp:send(Socket, TestMsg) of
                ok ->
                    io:format("✅ Test message sent~n"),
                    
                    % Try to receive response
                    case gen_tcp:recv(Socket, 0, 3000) of
                        {ok, Response} ->
                            io:format("✅ Received response: ~p~n", [Response]);
                        {error, timeout} ->
                            io:format("⚠️  No response received (timeout)~n");
                        {error, Reason} ->
                            io:format("❌ Receive error: ~p~n", [Reason])
                    end;
                {error, Reason} ->
                    io:format("❌ Send error: ~p~n", [Reason])
            end,
            
            gen_tcp:close(Socket),
            io:format("Connection closed~n");
        {error, Reason} ->
            io:format("❌ TCP connection failed: ~p~n", [Reason]),
            io:format("~nPossible issues:~n"),
            io:format("1. IB Gateway/TWS not running~n"),
            io:format("2. API not enabled in IB settings~n"),
            io:format("3. Port 7497 not configured~n"),
            io:format("4. Firewall blocking connection~n")
    end.