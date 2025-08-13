#!/usr/bin/env python3
"""
Simple EUR/USD Forex Trading Client for Interactive Brokers
Places a basic forex trade and monitors order status
"""

import time
import threading
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
from ibapi.order import Order


class ForexTrader(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.connected = False
        self.next_order_id = None
        self.order_status = {}
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        """Handle API errors"""
        print(f"Error {errorCode}: {errorString}")
        if errorCode == 202:  # Order cancelled
            print("Order was cancelled")
        elif errorCode == 201:  # Order rejected
            print("Order was rejected")
            
    def connectAck(self):
        """Called when connection is acknowledged"""
        print("Connection acknowledged")
        self.connected = True
        
    def nextValidId(self, orderId):
        """Receives next valid order ID"""
        print(f"Next valid order ID: {orderId}")
        self.next_order_id = orderId
        
    def managedAccounts(self, accountsList):
        """Receives list of managed accounts"""
        print(f"Managed accounts: {accountsList}")
        
    def orderStatus(self, orderId, status, filled, remaining, avgFillPrice, 
                   permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice):
        """Receives order status updates"""
        print(f"Order {orderId}: Status={status}, Filled={filled}, Remaining={remaining}, AvgPrice={avgFillPrice}")
        self.order_status[orderId] = {
            'status': status,
            'filled': filled,
            'remaining': remaining,
            'avgFillPrice': avgFillPrice
        }
        
    def openOrder(self, orderId, contract, order, orderState):
        """Receives open order information"""
        print(f"Open Order {orderId}: {contract.symbol} {order.action} {order.totalQuantity} @ {order.lmtPrice}")
        
    def execDetails(self, reqId, contract, execution):
        """Receives execution details"""
        print(f"Execution: {contract.symbol} {execution.side} {execution.shares} @ {execution.price}")
        
    def create_forex_contract(self, base_currency="EUR", quote_currency="USD"):
        """Create a forex contract"""
        contract = Contract()
        contract.symbol = base_currency
        contract.secType = "CASH"
        contract.currency = quote_currency
        contract.exchange = "IDEALPRO"  # Ideal for forex
        return contract
        
    def create_market_order(self, action, quantity):
        """Create a market order"""
        order = Order()
        order.action = action  # "BUY" or "SELL"
        order.orderType = "MKT"  # Market order
        order.totalQuantity = quantity
        return order
        
    def create_limit_order(self, action, quantity, limit_price):
        """Create a limit order"""
        order = Order()
        order.action = action  # "BUY" or "SELL"
        order.orderType = "LMT"  # Limit order
        order.totalQuantity = quantity
        order.lmtPrice = limit_price
        return order
        
    def place_forex_trade(self, action="BUY", quantity=25000, order_type="MKT", limit_price=None):
        """Place a forex trade"""
        if not self.next_order_id:
            print("No valid order ID available yet")
            return None
            
        # Create EUR/USD contract
        contract = self.create_forex_contract("EUR", "USD")
        
        # Create order based on type
        if order_type == "MKT":
            order = self.create_market_order(action, quantity)
            print(f"Placing {action} market order for {quantity} EUR/USD")
        else:
            if not limit_price:
                print("Limit price required for limit orders")
                return None
            order = self.create_limit_order(action, quantity, limit_price)
            print(f"Placing {action} limit order for {quantity} EUR/USD @ {limit_price}")
        
        # Place the order
        order_id = self.next_order_id
        self.placeOrder(order_id, contract, order)
        self.next_order_id += 1
        
        return order_id


def main():
    """Main function to run the forex trader"""
    print("Starting EUR/USD Forex Trader...")
    
    # Create trader instance
    trader = ForexTrader()
    
    # Connect to TWS/IB Gateway
    host = "127.0.0.1"
    port = 7497
    client_id = 2  # Different client ID to avoid conflicts
    
    print(f"Connecting to {host}:{port} with client ID {client_id}")
    trader.connect(host, port, client_id)
    
    # Start the socket in a separate thread
    api_thread = threading.Thread(target=trader.run, daemon=True)
    api_thread.start()
    
    # Wait for connection and next valid order ID
    timeout = 10
    start_time = time.time()
    while (not trader.connected or not trader.next_order_id) and (time.time() - start_time) < timeout:
        time.sleep(0.1)
    
    if not trader.connected:
        print("Failed to connect within timeout period")
        return
        
    if not trader.next_order_id:
        print("Failed to receive next valid order ID")
        return
        
    print("Successfully connected and ready to trade!")
    
    # Example trades - uncomment the one you want to test
    
    # Option 1: Small market buy order (25,000 EUR = ~$27,000)
    print("\n" + "="*50)
    print("PLACING FOREX TRADE")
    print("="*50)
    
    # CAUTION: This will place a real trade! 
    # Uncomment only when you're ready to trade
    
    # Market order example:
    # order_id = trader.place_forex_trade("BUY", 25000, "MKT")
    
    # Limit order example (adjust price as needed):
    # order_id = trader.place_forex_trade("BUY", 25000, "LMT", 1.0800)
    
    # For testing, let's just show what would happen:
    print("Trade setup ready!")
    print("To place a trade, uncomment one of the order lines in the code:")
    print("- Market order: trader.place_forex_trade('BUY', 25000, 'MKT')")
    print("- Limit order: trader.place_forex_trade('BUY', 25000, 'LMT', 1.0800)")
    
    # Wait a bit to see any order updates
    time.sleep(3)
    
    # Show order status if any orders were placed
    if trader.order_status:
        print("\nOrder Status Summary:")
        for order_id, status in trader.order_status.items():
            print(f"Order {order_id}: {status}")
    
    # Disconnect
    trader.disconnect()
    print("\nDisconnected from Interactive Brokers")


if __name__ == "__main__":
    main()