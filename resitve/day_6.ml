let predelaj_datoteko vsebina_datoteke =
  vsebina_datoteke
  |> String.trim
  |> String.split_on_char ','
  |> List.map int_of_string

let en_dan sez =
  let rec aux sez acc = match sez with
    | 8 :: rep -> aux rep (7 :: acc)
    | 7 :: rep -> aux rep (6 :: acc)
    | 6 :: rep -> aux rep (5 :: acc)
    | 5 :: rep -> aux rep (4 :: acc)
    | 4 :: rep -> aux rep (3 :: acc)
    | 3 :: rep -> aux rep (2 :: acc)
    | 2 :: rep -> aux rep (1 :: acc)
    | 1 :: rep -> aux rep (0 :: acc)
    | 0 :: rep -> aux rep (8 :: 6 :: acc)
    | [] -> acc
    | _ :: failwith -> [-1]
  in
  aux sez []

let n_ti_dan n sez =
  let rec aux n sez acc = 
    (if acc = n then sez else (if acc < n then aux n (en_dan sez) (acc+1) else failwith "Napaka."))
  in
  aux n sez 0
  
let prestej_po_n n sez =
  let rec aux n sez acc = match sez with
    | [] -> acc
    | x :: xs -> if x = n then aux n xs (acc+1) else aux n xs acc
  in
  aux n sez 0

let prestej_vse sez =
  let rec aux n sez acc = 
    if n >= 0 then aux (n-1) sez ((prestej_po_n n sez) :: acc) else acc
  in
  aux 8 sez []

let en_dan_hitreje sez = match sez with
  | [x0; x1; x2; x3; x4; x5; x6; x7; x8] -> [x1; x2; x3; x4; x5; x6; x7 + x0; x8; x0]
  | _ -> failwith "Napaka"

let n_ti_dan_hitreje n sez =
  let stevila_po_dnevih = prestej_vse sez
  in
  let rec aux n stevila acc =
    if acc = n then stevila else (if acc < n then aux n (en_dan_hitreje stevila) (acc+1) else failwith "Napaka")
  in
  aux n stevila_po_dnevih 0

let vsota sez =
  let rec aux sez acc = match sez with
    | [] -> acc
    | x::xs -> aux xs (acc + x)
  in
  aux sez 0

let naloga1 vsebina_datoteke =
  let sez = predelaj_datoteko vsebina_datoteke in
  string_of_int (List.length (n_ti_dan 80 sez))

let naloga2 vsebina_datoteke =
  let sez = predelaj_datoteko vsebina_datoteke in
  string_of_int (vsota (n_ti_dan_hitreje 256 sez))

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
  let vsebina_datoteke = preberi_datoteko "inputs/day_6.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "outputs/day_6_1.out" odgovor1;
  izpisi_datoteko "outputs/day_6_2.out" odgovor2