let predelaj_datoteko vsebina_datoteke = 
  vsebina_datoteke 
  |> String.split_on_char '\n' 
  |> List.map String.trim 
  |> List.map int_of_string

let increase_count sez =
  let rec aux sez acc = match sez with
    | [] -> acc
    | x :: [] -> acc
    | a :: b :: rep -> (if a < b then aux (b :: rep) (acc+1) else aux (b :: rep) acc)
  in
  aux sez 0

let naloga1 vsebina_datoteke =
  let sez = predelaj_datoteko vsebina_datoteke in
    string_of_int (increase_count sez)

let reverse sez =
  let rec aux sez acc = match sez with
    | [] -> acc
    | x :: xs -> aux xs (x :: acc)
  in
  aux sez []

let vsote_trojk sez =
  let rec aux sez acc = match sez with
    | [] -> acc
    | x :: [] -> acc
    | x :: y :: [] -> acc
    | a :: b :: c :: rep -> aux (b :: c :: rep) ((a + b + c) :: acc)
  in
  aux sez []

let naloga2 vsebina_datoteke =
  let sez = predelaj_datoteko vsebina_datoteke in
    string_of_int (increase_count (reverse (vsote_trojk sez)))

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
  let vsebina_datoteke = preberi_datoteko "inputs/day_1.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "outputs/day_1_1.out" odgovor1;
  izpisi_datoteko "outputs/day_1_2.out" odgovor2