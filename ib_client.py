#!/usr/bin/env python3
"""
Simple Interactive Brokers API client
Connects to TWS/IB Gateway and retrieves account information
"""

import time
import threading
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract


class IBClient(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.connected = False
        self.account_info = {}
        self.positions = []
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        """Handle API errors"""
        print(f"Error {errorCode}: {errorString}")
        
    def connectAck(self):
        """Called when connection is acknowledged"""
        print("Connection acknowledged")
        self.connected = True
        
    def nextValidId(self, orderId):
        """Receives next valid order ID"""
        print(f"Next valid order ID: {orderId}")
        self.start_requests()
        
    def managedAccounts(self, accountsList):
        """Receives list of managed accounts"""
        print(f"Managed accounts: {accountsList}")
        self.accounts = accountsList.split(",")
        
    def accountSummary(self, reqId, account, tag, value, currency):
        """Receives account summary data"""
        print(f"Account: {account}, {tag}: {value} {currency}")
        if account not in self.account_info:
            self.account_info[account] = {}
        self.account_info[account][tag] = {"value": value, "currency": currency}
        
    def accountSummaryEnd(self, reqId):
        """Called when account summary is complete"""
        print("Account summary complete")
        
    def position(self, account, contract, position, avgCost):
        """Receives position data"""
        if position != 0:  # Only show non-zero positions
            pos_info = {
                "account": account,
                "symbol": contract.symbol,
                "position": position,
                "avgCost": avgCost,
                "contract": contract
            }
            self.positions.append(pos_info)
            print(f"Position: {contract.symbol} - {position} @ {avgCost}")
            
    def positionEnd(self):
        """Called when position data is complete"""
        print("Position data complete")
        
    def start_requests(self):
        """Start requesting account data"""
        print("Starting data requests...")
        
        # Request account summary
        self.reqAccountSummary(1, "All", "AccountType,NetLiquidation,TotalCashValue,SettledCash,AccruedCash,BuyingPower,EquityWithLoanValue,PreviousEquityWithLoanValue,GrossPositionValue,ReqTEquity,ReqTMargin,SMA,InitMarginReq,MaintMarginReq,AvailableFunds,ExcessLiquidity,Cushion,FullInitMarginReq,FullMaintMarginReq,FullAvailableFunds,FullExcessLiquidity,LookAheadNextChange,LookAheadInitMarginReq,LookAheadMaintMarginReq,LookAheadAvailableFunds,LookAheadExcessLiquidity,HighestSeverity,DayTradesRemaining,Leverage")
        
        # Request positions
        self.reqPositions()


def main():
    """Main function to run the IB client"""
    print("Starting Interactive Brokers API client...")
    
    # Create client instance
    app = IBClient()
    
    # Connect to TWS/IB Gateway
    # Using your configured port 7497
    host = "127.0.0.1"
    port = 7497
    client_id = 1  # Using your Master API client ID
    
    print(f"Connecting to {host}:{port} with client ID {client_id}")
    app.connect(host, port, client_id)
    
    # Start the socket in a separate thread
    api_thread = threading.Thread(target=app.run, daemon=True)
    api_thread.start()
    
    # Wait for connection
    timeout = 10
    start_time = time.time()
    while not app.connected and (time.time() - start_time) < timeout:
        time.sleep(0.1)
    
    if not app.connected:
        print("Failed to connect within timeout period")
        return
        
    print("Successfully connected to Interactive Brokers!")
    
    # Keep the application running for a bit to receive data
    time.sleep(5)
    
    # Print summary
    print("\n" + "="*50)
    print("CONNECTION SUMMARY")
    print("="*50)
    print(f"Connected: {app.connected}")
    print(f"Accounts: {getattr(app, 'accounts', 'Not received')}")
    print(f"Account info entries: {len(app.account_info)}")
    print(f"Positions: {len(app.positions)}")
    
    if app.account_info:
        print("\nAccount Summary:")
        for account, data in app.account_info.items():
            print(f"  Account: {account}")
            for tag, info in data.items():
                print(f"    {tag}: {info['value']} {info['currency']}")
    
    # Disconnect
    app.disconnect()
    print("\nDisconnected from Interactive Brokers")


if __name__ == "__main__":
    main()