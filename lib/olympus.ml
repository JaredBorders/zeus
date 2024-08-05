module Id = struct
  type t = int

  let compare = Int.compare
  let to_string = Int.to_string
end

module Kind = struct
  type t =
    | Market
    | Limit
    | Stop

  let compare = compare

  let to_string = function
    | Market -> "Market"
    | Limit -> "Limit"
    | Stop -> "Stop"
  ;;
end

module Price = struct
  type t = int

  let compare = Int.compare
  let to_string = Int.to_string
  let add x y = x + y
  let sub x y = x - y
  let mul x y = x * y
  let div x y = x / y
  let modulo x y = x mod y
  let equal = Int.equal
  let not_equal x y = not @@ Int.equal x y
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( % ) = modulo
  let ( = ) = equal
  let ( <> ) = not_equal
end

module Quantity = struct
  type t = int

  let compare = Int.compare
  let to_string = Int.to_string
  let add x y = x + y
  let sub x y = x - y
  let mul x y = x * y
  let div x y = x / y
  let modulo x y = x mod y
  let equal = Int.equal
  let not_equal x y = not @@ Int.equal x y
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( % ) = modulo
  let ( = ) = equal
  let ( <> ) = not_equal
end

module Side = struct
  type t =
    | Bid
    | Ask

  let compare = compare

  let to_string = function
    | Bid -> "Bid"
    | Ask -> "Ask"
  ;;
end

module Order = struct
  exception Invalid_quantity

  type t =
    { id : Id.t
    ; kind : Kind.t
    ; side : Side.t
    ; price : Price.t
    ; quantity : Quantity.t
    }

  let create id kind side price quantity =
    { id; kind; side; price; quantity }
  ;;

  let fill t quantity =
    if quantity > t.quantity then raise Invalid_quantity;
    { t with quantity = t.quantity - quantity }
  ;;

  let filled t = t.quantity = 0
end

module Level = struct
  exception Wrong_price
  exception Absent_order of int

  module OrderMap = Map.Make (Int)

  type t =
    { price : Price.t
    ; quantity : Quantity.t
    ; bids : Order.t OrderMap.t
    ; asks : Order.t OrderMap.t
    }

  let bids t = OrderMap.bindings t.bids
  let asks t = OrderMap.bindings t.asks

  let find t id =
    match OrderMap.find_opt id t.bids with
    | Some order -> Some order
    | None ->
      (match OrderMap.find_opt id t.asks with
       | Some order -> Some order
       | None -> None)
  ;;

  let create price =
    { price
    ; quantity = 0
    ; bids = OrderMap.empty
    ; asks = OrderMap.empty
    }
  ;;

  let add t (order : Order.t) =
    if order.price <> t.price then raise Wrong_price;
    let quantity = t.quantity + order.quantity in
    match order.side with
    | Side.Bid ->
      { t with
        quantity
      ; bids = OrderMap.add order.id order t.bids
      }
    | Side.Ask ->
      { t with
        quantity
      ; asks = OrderMap.add order.id order t.asks
      }
  ;;

  let remove t id =
    let order =
      match find t id with
      | Some order -> order
      | None -> raise (Absent_order id)
    in
    let quantity = t.quantity - order.quantity in
    match order.side with
    | Side.Bid ->
      let bids = OrderMap.remove id t.bids in
      { t with quantity; bids }
    | Side.Ask ->
      let asks = OrderMap.remove id t.asks in
      { t with quantity; asks }
  ;;
end

module Trade = struct
  type t =
    { bid : Id.t
    ; ask : Id.t
    }

  let create (bid : Id.t) (ask : Id.t) = { bid; ask }
end

module Orderbook = struct
  exception Invalid_id
  exception Invalid_tick
  exception Invalid_bid
  exception Invalid_ask
  exception Invalid_trade

  module IdMap = Map.Make (Int)

  type tick = int

  type t =
    { ticks : tick
    ; mutable levels : Level.t array
    ; mutable ids : (Id.t, tick) Hashtbl.t
    }

  let create ticks =
    let ids = Hashtbl.create 0 in
    let levels =
      Array.init ticks (fun i -> Level.create i)
    in
    { ticks; levels; ids }
  ;;

  let find (id : Id.t) book =
    match Hashtbl.find_opt book.ids id with
    | Some tick -> tick
    | None -> raise Invalid_id
  ;;

  let place (order : Order.t) book =
    if Hashtbl.mem book.ids order.id then raise Invalid_id;
    Hashtbl.add book.ids order.id order.price;
    let tick = order.price in
    let level = book.levels.(tick) in
    let level' = Level.add level order in
    book.levels.(tick) <- level'
  ;;

  let cancel (id : Id.t) book =
    match Hashtbl.find_opt book.ids id with
    | Some tick ->
      let level = book.levels.(tick) in
      let level' = Level.remove level id in
      book.levels.(tick) <- level';
      Hashtbl.remove book.ids id
    | None -> raise Invalid_id
  ;;

  let volume (tick : tick) book =
    if tick < 0 || tick >= book.ticks
    then raise Invalid_tick
    else book.levels.(tick).quantity
  ;;

  let fill (bid : Order.t) (ask : Order.t) level book =
    let bid_quantity = bid.quantity in
    let ask_quantity = ask.quantity in
    let level' = Level.remove level bid.id in
    let level'' = Level.remove level' ask.id in
    match compare bid_quantity ask_quantity with
    | 0 ->
      Hashtbl.remove book.ids bid.id;
      Hashtbl.remove book.ids ask.id;
      level''
    | x when x < 0 ->
      Hashtbl.remove book.ids bid.id;
      let ask' = Order.fill ask bid_quantity in
      Level.add level'' ask'
    | _ ->
      Hashtbl.remove book.ids ask.id;
      let bid' = Order.fill bid ask_quantity in
      Level.add level'' bid'
  ;;

  let execute (trade : Trade.t) book =
    let tick'' = find trade.bid book in
    let tick' = find trade.ask book in
    let tick =
      if tick'' = tick' then tick' else raise Invalid_tick
    in
    let level = book.levels.(tick) in
    let bid = Level.find level trade.bid in
    let ask = Level.find level trade.ask in
    match bid, ask with
    | Some bid, Some ask ->
      if bid.side = Side.Ask then raise Invalid_bid;
      if ask.side = Side.Bid then raise Invalid_ask;
      let level' = fill bid ask level book in
      book.levels.(tick) <- level'
    | _ -> raise Invalid_trade
  ;;
end
