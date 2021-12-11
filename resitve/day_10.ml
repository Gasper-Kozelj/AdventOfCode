let razbij niz =
    let rec aux i max acc =
        if i = max then acc else aux (i + 1) max (String.make 1 (niz.[i]) :: acc)
    in
    List.rev (aux 0 (String.length niz) [])

let preveri sez =
    let rec aux sez acc = match sez with
        | [] -> "P"
        | ")" :: xs -> (match acc with
            | [] -> ")"
            | "(" :: ys -> aux xs ys
            | y :: ys -> ")")
        | "]" :: xs -> (match acc with
            | [] -> "]"
            | "[" :: ys -> aux xs ys
            | y :: ys -> "]")
        | "}" :: xs -> (match acc with
            | [] -> "}"
            | "{" :: ys -> aux xs ys
            | y :: ys -> "}")
        | ">" :: xs -> (match acc with
            | [] -> ">"
            | "<" :: ys -> aux xs ys
            | y :: ys -> ">")
        | "(" :: xs -> aux xs ("(" :: acc)
        | "[" :: xs -> aux xs ("[" :: acc)
        | "{" :: xs -> aux xs ("{" :: acc)
        | "<" :: xs -> aux xs ("<" :: acc)
        | x :: xs  -> x
    in
    aux sez []

let predelaj_datoteko vsebina_datoteke = 
    vsebina_datoteke
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map String.trim

(*
  (): 3
  []: 57
  {}: 1197
  <>: 25137
*)

let vsota_sez sez = 
    let rec aux sez acc = match sez with
        | [] -> acc
        | x :: xs -> aux xs (acc + x)
    in
    aux sez 0

let vrednosti oklepaj = match oklepaj with
    | ")" -> 3
    | "]" -> 57
    | "}" -> 1197
    | ">" -> 25137
    | _ -> 0

let naloga1 vsebina_datoteke =
    let sez = predelaj_datoteko vsebina_datoteke in
        sez
        |> List.map razbij
        |> List.map preveri
        |> List.map vrednosti
        |> vsota_sez
        |> string_of_int

let logicna_vrednost niz = match niz with
    | ")" -> false
    | "]" -> false
    | "}" -> false
    | ">" -> false
    | _ -> true

let precisti sez =
    let rec aux sez acc = match sez with
        | [] -> acc
        | x :: xs -> if logicna_vrednost (preveri x) then aux xs (x :: acc) else aux xs acc
    in
    List.rev (aux sez [])

let dopolni sez =
    let rec aux sez acc = match acc with
        | [] -> (
            match sez with
            | [] -> []
            | "(" :: xs -> aux xs (")" :: [])
            | "[" :: xs -> aux xs ("]" :: [])
            | "{" :: xs -> aux xs ("}" :: [])
            | "<" :: xs -> aux xs (">" :: [])
            | x :: xs  -> failwith "Napaka"
            )
        | glava :: rep -> (
            match sez with
            | [] -> acc
            | ")" :: xs -> (
                match glava with
                | ")" -> aux xs rep
                | _ -> failwith "Napaka"
                )
            | "]" :: xs -> (
                match glava with
                | "]" -> aux xs rep
                | _ -> failwith "Napaka"
                )
            | "}" :: xs -> (
                match glava with
                | "}" -> aux xs rep
                | _ -> failwith "Napaka"
                )
            | ">" :: xs -> (
                match glava with
                | ">" -> aux xs rep
                | _ -> failwith "Napaka"
                )
            | "(" :: xs -> aux xs (")" :: acc)
            | "[" :: xs -> aux xs ("]" :: acc)
            | "{" :: xs -> aux xs ("}" :: acc)
            | "<" :: xs -> aux xs (">" :: acc)
            | x :: xs  -> failwith "Napaka"
            )
    in
    aux sez []

(*
  (): 1
  []: 2
  {}: 3
  <>: 4
*)

let tockuj_oklepaj niz = match niz with
    | ")" -> 1
    | "]" -> 2
    | "}" -> 3
    | ">" -> 4
    | _ -> failwith "Napaka"

let tockuj_vrstico sez =
    let rec aux sez acc = match sez with
        | [] -> acc
        | x :: xs -> aux xs ((5 * acc) + tockuj_oklepaj x)
    in
    aux sez 0

let uredi sez =
    List.sort compare sez

let srednji_element sez =
    if (List.length sez) = 0 mod 2 then (List.nth sez ((List.length sez)/2 - 1))
    else (List.nth sez ((List.length sez - 1)/2))

let naloga2 vsebina_datoteke =
    vsebina_datoteke
    |> predelaj_datoteko
    |> List.map razbij
    |> precisti
    |> List.map dopolni
    |> List.map tockuj_vrstico
    |> uredi
    |> srednji_element
    |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "inputs/day_10.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "outputs/day_10_1.out" odgovor1;
    izpisi_datoteko "outputs/day_10_2.out" odgovor2