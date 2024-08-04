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
