open Core
open Zeus.Olympus
open Zeus.Json

let () = Random.self_init ()

let create_order id kind side price quantity =
  Order.create id kind side price quantity
;;

let generate_orders tick count =
  let rec aux n acc =
    if n = 0
    then acc
    else (
      let id = (tick * 10) + n in
      let kind =
        match Random.int 3 with
        | 0 -> Kind.Market
        | 1 -> Kind.Limit
        | _ -> Kind.Stop
      in
      let side =
        if Random.bool () then Side.Bid else Side.Ask
      in
      let quantity = 10 + Random.int 1000 in
      let order = create_order id kind side tick quantity in
      aux (n - 1) (order :: acc))
  in
  aux count []
;;

let orders =
  List.concat_map
    ~f:(fun (tick, count) -> generate_orders tick count)
    [ 0, 5
    ; 1, 5
    ; 2, 5
    ; 3, 5
    ; 4, 5
    ; 5, 5
    ; 6, 5
    ; 7, 5
    ; 8, 5
    ; 9, 5
    ]
;;

let place_orders orders orderbook =
  List.fold orders ~init:orderbook ~f:(fun book order ->
    Orderbook.place order book;
    book)
;;

let empty_orderbook = Orderbook.create 10
let orderbook = place_orders orders empty_orderbook
let orderbook_json = Json.orderbook_to_string orderbook

let () =
  print_endline
    "\n<><> zeus.0.0.1 🐪><><><><><><><><><><><><><><><><>\n";
  print_endline orderbook_json;
  print_endline "\n"
;;
