open Id
open Kind
open Side
open Price
open Quantity

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

  let modify t kind price quantity =
    { t with kind; price; quantity }
  ;;

  let fill t quantity =
    if quantity > t.quantity then raise Invalid_quantity;
    { t with quantity = t.quantity - quantity }
  ;;

  let filled t = t.quantity = 0
end
