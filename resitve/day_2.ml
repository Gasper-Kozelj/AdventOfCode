let reverse sez =
  let rec aux sez acc = match sez with
    | [] -> acc
    | x :: xs -> aux xs (x :: acc)
  in
  aux sez []

  let sez_v_par sez = match sez with
  | x :: y :: [] -> (x, int_of_string y)
  | _ -> failwith "Nekaj je narobe."

let razbij sez =
  let rec aux sez acc = match sez with
    | [] -> acc
    | x :: xs -> aux xs ((sez_v_par (String.split_on_char ' ' x)) :: acc)
  in
  reverse (aux sez [])

let predelaj_datoteko vsebina_datoteke = 
  vsebina_datoteke
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> razbij

let rec pojdi_po_poti (a, b) sez = match sez with
  | [] -> (a,b)
  | ("forward", y) :: rep -> pojdi_po_poti (a + y, b) rep
  | ("down", y) :: rep -> pojdi_po_poti (a, b + y) rep
  | ("up", y) :: rep -> pojdi_po_poti (a, b - y) rep
  | _ -> failwith "Nekaj je narobe."

let zmnozi_2 (a, b) =
  a * b

let zmnozi_3 (a, b, c) =
  a * b

let naloga1 vsebina_datoteke =
  let sez = predelaj_datoteko vsebina_datoteke in
    string_of_int (zmnozi_2 (pojdi_po_poti (0, 0) sez))

let rec pojdi_po_poti_2 (position, depth, aim) sez = match sez with
  | [] -> (position, depth, aim)
  | ("forward", y) :: rep -> pojdi_po_poti_2 (position + y, depth + (aim * y), aim) rep
  | ("down", y) :: rep -> pojdi_po_poti_2 (position, depth, aim + y) rep
  | ("up", y) :: rep -> pojdi_po_poti_2 (position, depth, aim - y) rep
  | _ -> failwith "Nekaj je narobe."

let naloga2 vsebina_datoteke =
  let sez = predelaj_datoteko vsebina_datoteke in
    string_of_int (zmnozi_3 (pojdi_po_poti_2 (0, 0, 0) sez))

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
  let vsebina_datoteke = preberi_datoteko "inputs/day_2.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "outputs/day_2_1.out" odgovor1;
  izpisi_datoteko "outputs/day_2_2.out" odgovor2