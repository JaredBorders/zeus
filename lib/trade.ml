open Side
open Order

module Trade = struct
  exception Invalid_bid
  exception Invalid_ask

  type t =
    { bid : Order.t
    ; ask : Order.t
    }

  let create (bid : Order.t) (ask : Order.t) =
    if bid.side <> Side.Bid then raise Invalid_bid;
    if ask.side <> Side.Ask then raise Invalid_ask;
    { bid; ask }
  ;;
end
