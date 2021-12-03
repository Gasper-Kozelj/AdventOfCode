let potenca_2 n =
  let rec aux n acc = match n with
    | 0 -> acc
    | m -> aux (n-1) (acc * 2)
  in
  aux n 1

let predelaj_datoteko vsebina_datoteke =
  vsebina_datoteke
  |> String.split_on_char '\n'
  |> List.map String.trim

let naloga1 vsebina_datoteke =
  "aaaaaa"

let naloga2 vsebina_datoteke =
  string_of_int (String.length vsebina_datoteke)

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
  let vsebina_datoteke = preberi_datoteko "inputs/day_3.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "outputs/day_3_1.out" odgovor1;
  izpisi_datoteko "outputs/day_3_2.out" odgovor2