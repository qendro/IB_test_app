FROM python:3.11-slim

WORKDIR /app

COPY requirements.txt .
RUN pip install -r requirements.txt

COPY forex_trader_live.py .

CMD ["python", "forex_trader_live.py"]