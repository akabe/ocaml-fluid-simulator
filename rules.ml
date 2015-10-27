let f bits =
  let select x y = if Random.bool () then x else y in
  let default () = Cell.rotr bits 3 in
  if bits land 0b0000001 = 0b0000001
  then if bits land 0b0000010 = 0b0000010
       then if bits land 0b0000100 = 0b0000100
            then if bits land 0b1111000 = 0b0011000 then 0b1110101
                 else default ()
            else if bits land 0b0111000 = 0b0011000
                 then if bits land 0b1000000 = 0b1000000
                      then select 0b1101101 0b1110110
                      else select 0b0101101 0b0110110
                 else default ()
       else if bits land 0b0000100 = 0b0000100
            then if bits land 0b0001000 = 0b0001000
                 then if bits land 0b0110000 = 0b0100000
                      then if bits land 0b1000000 = 0b1000000
                           then select 0b1110110 0b1011011
                           else select 0b0110110 0b0011011
                      else default ()
                 else if bits land 0b0010000 = 0b0010000
                      then if bits land 0b0100000 = 0b0100000
                           then if bits land 0b1000000 = 0b1000000
                                then 0b0011111
                                else select 0b1010110 0b1101001
                           else if bits land 0b1000000 = 0b1000000
                                then 0b1101010 else 0b0101010
                      else default ()
            else if bits land 0b0011000 = 0b0001000
                 then if bits land 0b0100000 = 0b0100000
                      then if bits land 0b1000000 = 0b1000000
                           then select 0b1010110 0b0110101
                           else select 0b1001010 0b0110010
                      else if bits land 0b1000000 = 0b1000000
                           then select 0b1100100 0b1010010
                           else select 0b0100100 0b0010010
                 else default ()
  else if bits land 0b0000010 = 0b0000010
       then if bits land 0b0000100 = 0b0000100
            then if bits land 0b0011000 = 0b0010000
                 then if bits land 0b0100000 = 0b0100000
                      then if bits land 0b1000000 = 0b1000000
                           then select 0b1101101 0b1011011
                           else select 0b0101101 0b0011011
                      else if bits land 0b1000000 = 0b1000000
                           then select 0b1101001 0b0110101 else default ()
                 else default ()
            else if bits land 0b0001000 = 0b0001000
                 then if bits land 0b0010000 = 0
                      then if bits land 0b0100000 = 0b0100000
                           then if bits land 0b1000000 = 0b1000000
                                then 0b1010101 else 0b0010101
                           else if bits land 0b1000000 = 0b1000000
                                then select 0b0101001 0b0110010
                                else 0b1100000
                      else default ()
                 else if bits land 0b0010000 = 0b0010000
                      then if bits land 0b0100000 = 0b0100000
                           then if bits land 0b1000000 = 0
                                then select 0b1001010 0b0101001
                                else default ()
                           else if bits land 0b1000000 = 0b1000000
                                then select 0b1100100 0b1001001
                                else select 0b0100100 0b0001001
                      else default ()
       else if bits land 0b0000100 = 0b0000100
            then if bits land 0b0111000 = 0b0100000
                 then if bits land 0b1000000 = 0b1000000
                      then select 0b1001001 0b1010010
                      else select 0b0001001 0b0010010
                 else default ()
            else if bits land 0b1111000 = 0b1100000 then 0b0001010
                 else default ()