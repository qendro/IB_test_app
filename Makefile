REBAR3 = rebar3

.PHONY: compile clean test shell deps

# Default target
all: compile

# Compile the project
compile:
	$(REBAR3) compile

# Get dependencies
deps:
	$(REBAR3) deps

# Clean build artifacts
clean:
	$(REBAR3) clean

# Run tests
test:
	$(REBAR3) eunit

# Start Erlang shell with project loaded
shell:
	$(REBAR3) shell

# Test IB connection
test-connection:
	erl -pa _build/default/lib/*/ebin -s forex_cli main test -s init stop

# Run safe trader
safe-trader:
	erl -pa _build/default/lib/*/ebin -s forex_cli main safe -s init stop

# Run live trader (WARNING: Places real trades!)
live-trader:
	erl -pa _build/default/lib/*/ebin -s forex_cli main live -s init stop

# Build release
release:
	$(REBAR3) release

# Help
help:
	@echo "Available targets:"
	@echo "  compile        - Compile the project"
	@echo "  deps           - Get dependencies"
	@echo "  clean          - Clean build artifacts"
	@echo "  test           - Run tests"
	@echo "  shell          - Start Erlang shell"
	@echo "  test-connection - Test IB connection"
	@echo "  safe-trader    - Run safe trader (no real trades)"
	@echo "  live-trader    - Run live trader (REAL trades!)"
	@echo "  release        - Build release"