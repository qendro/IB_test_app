# Interactive Brokers API Client

A simple Python application that connects to Interactive Brokers TWS/IB Gateway API to retrieve account information and positions.

## Setup

1. Install the required dependencies:
```bash
pip install -r requirements.txt
```

2. Make sure your TWS or IB Gateway is running and configured with:
   - Socket port: 7497
   - API connections enabled
   - Trusted IP: 127.0.0.1

## Usage

Run the client:
```bash
python ib_client.py
```

The application will:
- Connect to your IB API on port 7497
- Establish a handshake
- Request account summary information
- Request current positions
- Display the results
- Disconnect cleanly

## Features

- Connects using your configured settings (port 7497, client ID 1)
- Retrieves comprehensive account summary data
- Shows current positions (non-zero only)
- Handles connection errors gracefully
- Clean disconnect

## Configuration

The client is configured to match your IB API settings:
- Host: 127.0.0.1
- Port: 7497
- Client ID: 1 (Master API client ID)

Make sure TWS/IB Gateway is running before executing the script.