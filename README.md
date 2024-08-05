# ⚡️ Zeus

## What is Zeus?

Zeus is a limit order book (LOB) implemented in OCaml. It leverages the power of a functional design to create a safe and reliable system for managing an order book. It includes modules for managing orders, price levels, trades, and an order book. Each component is built with immutability in mind, ensuring that the system remains consistent and predictable under all circumstances.

## Core Components

### Order Module

Manages individual orders with support for creating, filling, and checking the status of orders.

### Level Module

Handles price levels, aggregating orders at the same price (or tick) and providing efficient access and ~modification~ operations.

### Trade Module

Represents trades between orders, ensuring accurate tracking and execution.

### Orderbook Module

Manages the entire order book, including placing, canceling, and executing orders. It maintains separate records for bids and asks, ensuring efficient order matching and book updates.

## Benefits of Being Functional

-   **Maintainability**: Code is easier to understand and maintain due to clear separation of data and behavior.
-   **Reliability**: Reduced side effects lead to fewer bugs and more predictable system behavior.
-   **Scalability**: Functional code scales well with increased complexity and concurrent operations.

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
