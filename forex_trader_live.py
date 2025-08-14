#!/usr/bin/env python3
"""
Live EUR/USD Forex Trading Client - PLACES ACTUAL TRADES
Use with caution - this will place real trades!
"""

import time
import threading
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
from ibapi.order import Order


class LiveForexTrader(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.connected = False
        self.next_order_id = None
        self.order_status = {}
        self.executions = []
        
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
        self.next_order_id = orderId
        
    def orderStatus(self, orderId, status, filled, remaining, avgFillPrice, 
                   permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice):
        """Receives order status updates"""
        print(f"📊 Order {orderId}: Status={status}, Filled={filled}, Remaining={remaining}")
        if avgFillPrice > 0:
            print(f"   Average Fill Price: {avgFillPrice}")
        self.order_status[orderId] = {
            'status': status,
            'filled': filled,
            'remaining': remaining,
            'avgFillPrice': avgFillPrice
        }
        
    def openOrder(self, orderId, contract, order, orderState):
        """Receives open order information"""
        print(f"📋 Open Order {orderId}: {contract.symbol}/{contract.currency} {order.action} {order.totalQuantity}")
        
    def execDetails(self, reqId, contract, execution):
        """Receives execution details"""
        print(f"✅ EXECUTION: {contract.symbol}/{contract.currency} {execution.side} {execution.shares} @ {execution.price}")
        print(f"   Execution ID: {execution.execId}, Time: {execution.time}")
        self.executions.append({
            'symbol': contract.symbol,
            'currency': contract.currency,
            'side': execution.side,
            'shares': execution.shares,
            'price': execution.price,
            'time': execution.time
        })


def create_eurusd_contract():
    """Create EUR/USD forex contract"""
    contract = Contract()
    contract.symbol = "EUR"
    contract.secType = "CASH"
    contract.currency = "USD"
    contract.exchange = "IDEALPRO"
    return contract


def create_market_order(action, quantity):
    """Create a market order"""
    order = Order()
    order.action = action
    order.orderType = "MKT"
    order.totalQuantity = quantity
    order.eTradeOnly = False  # Explicitly set to False for compatibility
    order.firmQuoteOnly = False
    return order


def main():
    """Main trading function"""
    print("🚨 LIVE EUR/USD FOREX TRADER 🚨")
    print("Auto-executing small test trade...")
    print("="*50)
    
    # Auto-set trade parameters for testing
    action = "BUY"
    quantity = 1000  # Small test size: 1,000 EUR (~$1,100)
    
    print(f"\n📋 Trade Setup:")
    print(f"   Pair: EUR/USD")
    print(f"   Action: {action}")
    print(f"   Quantity: {quantity:,} EUR")
    print(f"   Order Type: Market")
    print("   Auto-executing in 2 seconds...")
    
    time.sleep(2)
    
    # Create trader
    trader = LiveForexTrader()
    
    # Connect
    print(f"\n🔌 Connecting to IB API...")
    trader.connect("host.docker.internal", 7497, 3)
    
    # Start API thread
    api_thread = threading.Thread(target=trader.run, daemon=True)
    api_thread.start()
    
    # Wait for connection
    timeout = 10
    start_time = time.time()
    while (not trader.connected or not trader.next_order_id) and (time.time() - start_time) < timeout:
        time.sleep(0.1)
    
    if not trader.connected or not trader.next_order_id:
        print("❌ Failed to connect or get order ID")
        return
    
    print("✅ Connected and ready to trade!")
    
    # Place the trade
    print(f"\n🚀 PLACING TRADE...")
    contract = create_eurusd_contract()
    order = create_market_order(action, quantity)
    
    order_id = trader.next_order_id
    trader.placeOrder(order_id, contract, order)
    
    print(f"📤 Order {order_id} submitted!")
    
    # Monitor for 10 seconds
    print("\n📊 Monitoring order status...")
    for i in range(10):
        time.sleep(1)
        if order_id in trader.order_status:
            status = trader.order_status[order_id]['status']
            if status in ['Filled', 'Cancelled']:
                break
        print(f"   Waiting... ({i+1}/10)")
    
    # Final status
    print("\n" + "="*50)
    print("TRADE SUMMARY")
    print("="*50)
    
    if order_id in trader.order_status:
        status_info = trader.order_status[order_id]
        print(f"Order Status: {status_info['status']}")
        print(f"Filled: {status_info['filled']}")
        print(f"Remaining: {status_info['remaining']}")
        if status_info['avgFillPrice'] > 0:
            print(f"Average Fill Price: {status_info['avgFillPrice']}")
    
    if trader.executions:
        print(f"\nExecutions ({len(trader.executions)}):")
        for exec_info in trader.executions:
            print(f"  {exec_info['side']} {exec_info['shares']} {exec_info['symbol']}/{exec_info['currency']} @ {exec_info['price']}")
    
    trader.disconnect()
    print("\n✅ Disconnected from IB")


if __name__ == "__main__":
    main()