module Trade = struct
  type details =
    { id : int
    ; price : float
    ; quantity : int
    }

  type t =
    { bid : details
    ; ask : details
    }

  let create bid ask = { bid; ask }
end
