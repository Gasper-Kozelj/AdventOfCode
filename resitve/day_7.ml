let predelaj_datoteko vsebina_datoteke =
  vsebina_datoteke
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.map int_of_string

let razlika a b =
  if a >= b then a - b else b - a 

let vsota_razlik_po_k k sez = 
  let rec aux n sez acc = match sez with
    | [] -> acc
    | x::xs -> aux n xs (acc + (razlika x n))
  in
  aux k sez 0

let minimum (glava :: rep) =
  let rec aux sez acc = match sez with
    | [] -> acc
    | x::xs -> (if x < acc then aux xs x else aux xs acc)
  in
  aux rep glava

let maximum (glava :: rep) =
  let rec aux sez acc = match sez with
    | [] -> acc
    | x::xs -> (if x > acc then aux xs x else aux xs acc)
  in
  aux rep glava

let vsote_za_vse_k n m sez =
  let rec aux n m sez acc k =
    if k <= m then (aux n m sez (vsota_razlik_po_k k sez :: acc) (k + 1)) else acc
  in 
  aux n m sez [] n

let naloga1 vsebina_datoteke =
  let sez = predelaj_datoteko vsebina_datoteke in 
  string_of_int (minimum (vsote_za_vse_k (minimum sez) (maximum sez) sez))

let razlika_2 a b =
  let n = razlika a b in
    if n = 0 then 0 else (n * (n + 1)) / 2

let vsota_razlik_po_k_2 k sez = 
  let rec aux k sez acc= match sez with
    | [] -> acc
    | x::xs -> aux k xs (acc + (razlika_2 x k))
  in
  aux k sez 0

let vsote_za_vse_k_2 n m sez =
  let rec aux n m sez acc k =
    if k <= m then (aux n m sez (vsota_razlik_po_k_2 k sez :: acc) (k + 1)) else acc
  in 
  aux n m sez [] n

let naloga2 vsebina_datoteke =
  let sez = predelaj_datoteko vsebina_datoteke in 
  string_of_int (minimum (vsote_za_vse_k_2 (minimum sez) (maximum sez) sez))

let _ =
  let preberi_datoteko ime_datoteke =
      let chan = open_in ime_datoteke in
      let vsebina = really_input_string chan (in_channel_length chan) in
      close_in chan;
      vsebina
  and izpisi_datoteko ime_datoteke vsebina =
      let chan = open_out ime_datoteke in
      output_string chan vsebina;
      close_out chan
  in
  let vsebina_datoteke = preberi_datoteko "inputs/day_7.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "outputs/day_7_1.out" odgovor1;
  izpisi_datoteko "outputs/day_7_2.out" odgovor2