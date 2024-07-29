open Order
open Level

module OrderBook = struct
  exception NotImplemented

  type t =
    { bids : Level.t list
    ; asks : Level.t list
    }

  let create () = { bids = []; asks = [] }

  let add (order : Order.t) book =
    match order, book with
    | _ -> raise NotImplemented
  ;;

  let remove id book =
    match id, book with
    | _ -> raise NotImplemented
  ;;

  let fill bid ask book =
    match bid, ask, book with
    | _ -> raise NotImplemented
  ;;
end
