open Zeus.Olympus

module TestValues = struct
  (* Order 1 *)
  let order_id = 1
  let order_kind = Kind.Market
  let order_side_bid = Side.Bid
  let order_side_ask = Side.Ask
  let order_price = 90_000
  let order_quantity = 100

  (* Order 2 *)
  let order'_id = 2
  let order'_kind = Kind.Market
  let order'_side = Side.Ask
  let order'_price = 90_000
  let order'_quantity = 100

  (* Order 3 *)
  let order''_id = 3
  let order''_kind = Kind.Market
  let order''_side = Side.Bid
  let order''_price = 90_000
  let order''_quantity = 50

  (* Order 4 *)
  let order'''_id = 4
  let order'''_kind = Kind.Market
  let order'''_side = Side.Ask
  let order'''_price = 90_000
  let order'''_quantity = 20

  (* Order book *)
  let ticks = 1_000_000
end

open TestValues

module OrderTests = struct
  let%test "order create bid" =
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    order.id = order_id
    && order.kind = order_kind
    && order.side = order_side_bid
    && order.price = order_price
    && order.quantity = order_quantity
  ;;

  let%test "order create ask" =
    let order =
      Order.create
        order'_id
        order'_kind
        order'_side
        order'_price
        order'_quantity
    in
    order.id = order'_id
    && order.kind = order'_kind
    && order.side = order'_side
    && order.price = order'_price
    && order.quantity = order'_quantity
  ;;

  let%test "order fill full" =
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    let order' = Order.fill order order_quantity in
    order'.quantity = 0
  ;;

  let%test "order fill partial" =
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    let partial_quantity = order_quantity / 2 in
    let order' = Order.fill order partial_quantity in
    order'.quantity = order_quantity - partial_quantity
  ;;

  let%test "order filled false" =
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    Order.filled order = false
  ;;

  let%test "order filled true" =
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    let order' = Order.fill order order_quantity in
    Order.filled order' = true
  ;;
end

module LevelTests = struct
  let%test "level create" =
    let level = Level.create order_price in
    level.price = order_price
    && level.quantity = 0
    && Level.bids level = []
    && Level.asks level = []
  ;;

  let%test "level add order" =
    let level = Level.create order_price in
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    let level' = Level.add level order in
    level'.quantity = order_quantity
    && Level.bids level' = [ order_id, order ]
  ;;

  let%test "level add order wrong price" =
    let level = Level.create order_price in
    let wrong_price = order_price + 1 in
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        wrong_price
        order_quantity
    in
    try
      let _ = Level.add level order in
      false
    with
    | Level.Wrong_price -> true
  ;;

  let%test "level remove order" =
    let level = Level.create order_price in
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    let level' = Level.add level order in
    let level'' = Level.remove level' order_id in
    level''.quantity = 0 && Level.bids level'' = []
  ;;

  let%test "level remove order absent" =
    let level = Level.create order_price in
    try
      let _ = Level.remove level order_id in
      false
    with
    | Level.Absent_order _ -> true
  ;;
end

module TradeTests = struct
  let%test "trade create" =
    let trade = Trade.create order_id order'_id in
    trade.bid = order_id && trade.ask = order'_id
  ;;
end

module OrderbookTests = struct
  let%test "orderbook create" =
    let book = Orderbook.create ticks in
    book.ticks = ticks
    && Array.length book.levels = ticks
    && Hashtbl.length book.ids = 0
  ;;

  let%test "orderbook find_tick" =
    let book = Orderbook.create ticks in
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    Orderbook.place order book;
    Orderbook.find order_id book = order_price
  ;;

  let%test "orderbook place" =
    let book = Orderbook.create ticks in
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    Orderbook.place order book;
    Hashtbl.find_opt book.ids order_id = Some order_price
    && book.levels.(order_price).quantity = order_quantity
  ;;

  let%test "orderbook place duplicate id" =
    let book = Orderbook.create ticks in
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    Orderbook.place order book;
    let order_duplicate =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    try
      Orderbook.place order_duplicate book;
      false
    with
    | Orderbook.Invalid_id -> true
  ;;

  let%test "orderbook cancel" =
    let book = Orderbook.create ticks in
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    Orderbook.place order book;
    Orderbook.cancel order_id book;
    Hashtbl.find_opt book.ids order_id = None
    && book.levels.(order_price).quantity = 0
  ;;

  let%test "orderbook cancel invalid id" =
    let book = Orderbook.create ticks in
    try
      Orderbook.cancel order_id book;
      false
    with
    | Orderbook.Invalid_id -> true
  ;;

  let%test "orderbook volume" =
    let book = Orderbook.create ticks in
    let order =
      Order.create
        order_id
        order_kind
        order_side_bid
        order_price
        order_quantity
    in
    Orderbook.place order book;
    Orderbook.volume order_price book = order_quantity
  ;;

  let%test "orderbook volume invalid negative tick" =
    let invalid_tick = -ticks in
    let book = Orderbook.create ticks in
    try
      let _ = Orderbook.volume invalid_tick book in
      false
    with
    | Orderbook.Invalid_tick -> true
  ;;

  let%test "orderbook volume invalid tick exceeds ticks" =
    let invalid_tick = ticks + 1 in
    let book = Orderbook.create ticks in
    try
      let _ = Orderbook.volume invalid_tick book in
      false
    with
    | Orderbook.Invalid_tick -> true
  ;;

  let%test "orderbook fill" =
    let book = Orderbook.create ticks in
    let bid =
      Order.create
        order_id
        Kind.Limit
        order_side_bid
        order_price
        order_quantity
    in
    let ask =
      Order.create
        order'_id
        Kind.Limit
        order_side_ask
        order_price
        order'_quantity
    in
    Orderbook.place bid book;
    Orderbook.place ask book;
    let level = book.levels.(order_price) in
    let level' = Orderbook.fill bid ask level book in
    level'.quantity = 0
  ;;

  let%test "orderbook execute" =
    let book = Orderbook.create ticks in
    let bid =
      Order.create
        order_id
        Kind.Limit
        order_side_bid
        order_price
        order_quantity
    in
    let ask =
      Order.create
        order'_id
        Kind.Limit
        order_side_ask
        order_price
        order'_quantity
    in
    Orderbook.place bid book;
    Orderbook.place ask book;
    let trade = Trade.create order_id order'_id in
    Orderbook.execute trade book;
    book.levels.(order_price).quantity = 0
  ;;

  let%test "orderbook execute invalid tick" =
    let book = Orderbook.create ticks in
    let bid =
      Order.create
        order_id
        Kind.Limit
        order_side_bid
        order_price
        order_quantity
    in
    let ask =
      Order.create
        order'_id
        Kind.Limit
        order_side_ask
        (order_price + 1)
        order'_quantity
    in
    Orderbook.place bid book;
    Orderbook.place ask book;
    let trade = Trade.create order_id order'_id in
    try
      Orderbook.execute trade book;
      false
    with
    | Orderbook.Invalid_tick -> true
  ;;

  let%test "orderbook execute invalid bid" =
    let book = Orderbook.create ticks in
    let bid =
      Order.create
        order_id
        Kind.Limit
        order_side_ask
        order_price
        order_quantity
    in
    let ask =
      Order.create
        order'_id
        Kind.Limit
        order_side_ask
        order_price
        order'_quantity
    in
    Orderbook.place bid book;
    Orderbook.place ask book;
    let trade = Trade.create order_id order'_id in
    try
      Orderbook.execute trade book;
      false
    with
    | Orderbook.Invalid_bid -> true
  ;;

  let%test "orderbook execute invalid ask" =
    let book = Orderbook.create ticks in
    let bid =
      Order.create
        order_id
        Kind.Limit
        order_side_bid
        order_price
        order_quantity
    in
    let ask =
      Order.create
        order'_id
        Kind.Limit
        order_side_bid
        order_price
        order'_quantity
    in
    Orderbook.place bid book;
    Orderbook.place ask book;
    let trade = Trade.create order_id order'_id in
    try
      Orderbook.execute trade book;
      false
    with
    | Orderbook.Invalid_ask -> true
  ;;
end
