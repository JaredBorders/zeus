# ⚡️ Zeus

## What is Zeus?

Zeus is a limit order book (LOB) implemented in OCaml 🐪

## Core Components

### Order Module

Manages individual orders with support for creating, filling, and checking the status of orders.

### Level Module

Handles price levels, aggregating orders at the same price (or tick) and providing efficient access and manipulation.

### Trade Module

Handles a trade consisting of two orders that can be processed by the order book, ensuring accurate tracking and execution.

### Orderbook Module

Manages the entire order book, including placing, canceling, and executing orders. It maintains separate records for bids and asks, ensuring efficient order matching and book updates.

## λ Why Functional?

Side effects are bad 🙅‍♂️

## Getting Started

build:

```
make build
```

run:

```
make run
```

test:

```
make test
```

format:

```
make fmt
```
