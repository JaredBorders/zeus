module Id = struct
  type t = int

  let compare = Int.compare
  let to_string = Int.to_string
end
