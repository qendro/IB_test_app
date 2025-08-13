# Interactive Brokers API Trading System Architecture

## System Overview

This system provides a Python-based interface to Interactive Brokers' TWS/IB Gateway API for automated trading, specifically designed for forex trading with EUR/USD pairs. The architecture follows a client-wrapper pattern using IB's official Python API.

## Core Components

### 1. Base Client (`ib_client.py`)
**Purpose**: Account information retrieval and connection testing
**Architecture**: Inherits from both `EWrapper` and `EClient`

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   ib_client.py  │───▶│  IB API Socket   │───▶│  TWS/Gateway    │
│   (EWrapper +   │    │  (Port 7497)     │    │  (Trading App)  │
│    EClient)     │◀───│                  │◀───│                 │
└─────────────────┘    └──────────────────┘    └─────────────────┘
```

**Key Methods**:
- `connectAck()`: Handles connection acknowledgment
- `nextValidId()`: Receives next available order ID
- `accountSummary()`: Processes account data
- `position()`: Handles position information

### 2. Forex Trading Clients

#### Safe Trader (`forex_trader.py`)
**Purpose**: Testing and development without placing real trades
**Features**: 
- Connection testing
- Order preparation
- Trade simulation setup

#### Live Trader (`forex_trader_live.py`)
**Purpose**: Production trading with real order execution
**Features**:
- Automatic trade execution
- Real-time order monitoring
- Execution reporting

## Data Flow Architecture

### Connection Flow
```
1. Client Creation
   ├── Initialize EWrapper/EClient
   ├── Set connection parameters
   └── Create threading context

2. API Connection
   ├── Connect to localhost:7497
   ├── Authenticate with client ID
   └── Wait for acknowledgment

3. Handshake Process
   ├── Receive connectAck()
   ├── Get nextValidId()
   ├── Receive managedAccounts()
   └── Connection ready
```

### Trading Flow
```
1. Order Preparation
   ├── Create Contract (EUR/USD)
   │   ├── Symbol: "EUR"
   │   ├── SecType: "CASH"
   │   ├── Currency: "USD"
   │   └── Exchange: "IDEALPRO"
   │
   ├── Create Order
   │   ├── Action: "BUY"/"SELL"
   │   ├── OrderType: "MKT"
   │   ├── TotalQuantity: amount
   │   └── Additional attributes
   │
   └── Assign Order ID

2. Order Execution
   ├── placeOrder(id, contract, order)
   ├── Monitor orderStatus() callbacks
   ├── Track execDetails() for fills
   └── Update internal state

3. Order Lifecycle
   ├── PreSubmitted → Submitted → Filled
   ├── Error handling for rejections
   └── Execution confirmation
```

## Threading Architecture

### Main Thread
- User interface and control logic
- Connection management
- Order placement commands

### API Thread
- Socket communication with IB
- Callback processing
- Real-time data handling

```
┌─────────────────┐    ┌─────────────────┐
│   Main Thread   │    │   API Thread    │
│                 │    │                 │
│ ┌─────────────┐ │    │ ┌─────────────┐ │
│ │ User Logic  │ │    │ │ Socket I/O  │ │
│ │ Order Mgmt  │ │────┼▶│ Callbacks   │ │
│ │ Monitoring  │ │    │ │ Data Stream │ │
│ └─────────────┘ │    │ └─────────────┘ │
└─────────────────┘    └─────────────────┘
```

## Contract Specifications

### EUR/USD Forex Contract
```python
contract = Contract()
contract.symbol = "EUR"        # Base currency
contract.secType = "CASH"      # Forex instrument type
contract.currency = "USD"      # Quote currency
contract.exchange = "IDEALPRO" # Optimal forex exchange
```

**Exchange Selection**:
- `IDEALPRO`: Best for amounts ≥20,000 EUR
- Smaller amounts routed as "odd lots"
- Automatic routing for optimal execution

## Order Types and Attributes

### Market Orders
```python
order = Order()
order.action = "BUY"/"SELL"
order.orderType = "MKT"
order.totalQuantity = amount
order.eTradeOnly = False      # Compatibility
order.firmQuoteOnly = False   # Compatibility
```

### Order States
1. **PreSubmitted**: Order received, pending validation
2. **Submitted**: Order sent to exchange
3. **Filled**: Order executed (partial or complete)
4. **Cancelled**: Order cancelled
5. **Error**: Order rejected

## Error Handling Strategy

### Connection Errors
- Timeout handling (10-second default)
- Retry logic for failed connections
- Graceful degradation

### Trading Errors
- Order rejection handling
- Market data validation
- Position size warnings

### Common Error Codes
- `2104`: Market data farm connection OK
- `2106`: HMDS data farm connection OK
- `399`: Order warnings (size, routing)
- `10268`: Attribute compatibility issues

## Security and Risk Management

### Connection Security
- Localhost-only connections (127.0.0.1)
- Client ID isolation
- Trusted IP configuration

### Trading Controls
- Minimum position sizes enforced by exchange
- Order validation before submission
- Real-time execution monitoring

## Configuration Management

### IB Gateway/TWS Settings
```
API Settings:
├── Socket Port: 7497
├── Client ID: 1-3 (different per client)
├── Read-Only API: Disabled
├── Trusted IPs: 127.0.0.1
└── API Logging: Enabled
```

### Client Configuration
```python
# Connection parameters
host = "127.0.0.1"
port = 7497
client_id = unique_per_client

# Trading parameters
base_currency = "EUR"
quote_currency = "USD"
exchange = "IDEALPRO"
```

## Monitoring and Logging

### Real-time Monitoring
- Order status updates via callbacks
- Execution details tracking
- Account balance monitoring
- Position tracking

### Logging Strategy
- Connection events
- Order lifecycle tracking
- Error logging with codes
- Execution confirmations

## Scalability Considerations

### Multiple Clients
- Different client IDs prevent conflicts
- Parallel execution capability
- Independent order tracking

### Performance Optimization
- Asynchronous callback handling
- Efficient threading model
- Minimal latency order placement

## Cross-Platform Compatibility

### Supported Platforms
- Windows (PowerShell/CMD)
- macOS (Terminal/Bash)
- Linux (Bash)

### Dependencies
- Python 3.7+
- ibapi library (9.81.1+)
- Standard library threading
- Platform-agnostic socket communication

## Deployment Architecture

```
Development Environment:
├── ib_client.py (testing)
├── forex_trader.py (safe testing)
└── forex_trader_live.py (production)

Production Environment:
├── Live IB Gateway/TWS
├── Real account credentials
├── Production monitoring
└── Risk management controls
```

This architecture provides a robust, scalable foundation for automated forex trading while maintaining clear separation between testing and production environments.