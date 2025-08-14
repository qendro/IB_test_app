# Interactive Brokers Forex Trading System - Erlang Implementation

This is an Erlang implementation of the Python-based Interactive Brokers forex trading system. It provides the same core functionality for automated EUR/USD forex trading using IB's TWS/Gateway API.

## Architecture

The Erlang implementation follows the same architectural patterns as the Python version:

### Core Modules

1. **ib_client.erl** - Base IB API client (equivalent to ib_client.py)
   - Handles TCP connection to IB Gateway/TWS
   - Processes API messages and callbacks
   - Manages account data and positions

2. **forex_trader.erl** - Safe testing trader (equivalent to forex_trader.py)
   - Connection testing and order preparation
   - Trade simulation without real execution
   - Development and testing environment

3. **forex_trader_live.erl** - Live production trader (equivalent to forex_trader_live.py)
   - Real trade execution capabilities
   - Order monitoring and execution reporting
   - Production trading environment

4. **forex_cli.erl** - Command-line interface
   - Easy access to different trading modes
   - Test connection functionality
   - Interactive trading commands

## Prerequisites

- Erlang/OTP 24+ installed
- Rebar3 build tool
- Interactive Brokers TWS or IB Gateway running
- IB Gateway configured with:
  - Socket port: 7497
  - API enabled
  - Trusted IP: 127.0.0.1

## Installation

1. Clone or create the project directory
2. Install dependencies:
   ```bash
   make deps
   ```

3. Compile the project:
   ```bash
   make compile
   ```

## Configuration

Edit `config/sys.config` to match your IB setup:

```erlang
[
 {forex_trader, [
   {ib_host, "127.0.0.1"},
   {ib_port, 7497},
   {client_id, 1}
 ]}
].
```

## Usage

### Quick Start Commands

```bash
# Test IB connection
make test-connection

# Run safe trader (no real trades)
make safe-trader

# Run live trader (WARNING: Places real trades!)
make live-trader
```

### Interactive Shell

Start the Erlang shell with the project loaded:

```bash
make shell
```

Then use the CLI functions:

```erlang
% Test connection
forex_cli:test_connection().

% Safe trader
forex_cli:safe_trader().

% Live trader (CAUTION!)
forex_cli:live_trader().
```

### Manual Trading

For more control, use the modules directly:

```erlang
% Start and connect IB client
{ok, Client} = ib_client:start_link("127.0.0.1", 7497, 1).
ib_client:connect(Client).

% Start forex trader
{ok, Trader} = forex_trader:start_link().
forex_trader:connect(Trader).

% Place test trade (safe mode)
forex_trader:place_forex_trade(Trader, buy, 25000, market).

% Check order status
forex_trader:get_order_status(Trader).

% Disconnect
forex_trader:disconnect(Trader).
ib_client:disconnect(Client).
```

## Trading Examples

### Market Orders

```erlang
% Buy 25,000 EUR at market price
forex_trader:place_forex_trade(Trader, buy, 25000, market).

% Sell 25,000 EUR at market price  
forex_trader:place_forex_trade(Trader, sell, 25000, market).
```

### Limit Orders

```erlang
% Buy 25,000 EUR with limit price 1.0800
forex_trader:place_forex_trade(Trader, buy, 25000, {limit, 1.0800}).

% Sell 25,000 EUR with limit price 1.0900
forex_trader:place_forex_trade(Trader, sell, 25000, {limit, 1.0900}).
```

## Safety Features

### Safe Mode (Default)
- The `forex_trader.erl` module runs in safe mode by default
- Orders are prepared but not actually sent to IB
- Perfect for testing and development
- Shows what trades would be placed

### Live Mode (Production)
- The `forex_trader_live.erl` module can place real trades
- Currently configured in simulation mode for safety
- To enable real trading, uncomment the `ib_client:place_order/4` calls
- Always test thoroughly before enabling live trading

## Error Handling

The system handles common errors:

- Connection timeouts
- Invalid order parameters  
- Market data issues
- Order rejections
- Network disconnections

## Development

### Building
```bash
make compile
```

### Testing
```bash
make test
```

### Cleaning
```bash
make clean
```

## Differences from Python Version

1. **Concurrency Model**: Uses Erlang's actor model with gen_server behaviors
2. **Error Handling**: Leverages Erlang's "let it crash" philosophy with supervisor trees
3. **Message Passing**: Uses Erlang's native message passing instead of callbacks
4. **Type Safety**: Uses Erlang's pattern matching for message parsing
5. **Hot Code Loading**: Supports live code updates without stopping the system

## Security Notes

- Only connects to localhost (127.0.0.1) by default
- Uses different client IDs to prevent conflicts
- Safe mode prevents accidental trade execution
- All real trading functionality is clearly marked and disabled by default

## Troubleshooting

### Connection Issues
- Ensure IB Gateway/TWS is running
- Check port 7497 is open and configured
- Verify API is enabled in IB settings
- Confirm trusted IP includes 127.0.0.1

### Compilation Issues
- Ensure Erlang/OTP 24+ is installed
- Run `make deps` to install dependencies
- Check rebar3 is available in PATH

### Trading Issues
- Verify account has sufficient funds
- Check minimum trade sizes (typically 20,000+ EUR for IDEALPRO)
- Ensure market is open for forex trading
- Review IB error messages in console output

## License

This implementation follows the same license as the original Python version.

## Disclaimer

This software is for educational and testing purposes. Always test thoroughly with paper trading before using with real money. Trading involves risk of loss.