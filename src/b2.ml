open Core

type mode =
  | Binary
  | Hex
  | Ascii
  | Snarky

let mode_of_string s =
  List.find_map_exn [ ("snarky", Snarky); ("ascii", Ascii); ("binary", Binary); ("hex", Hex) ] ~f:(fun (m, mode) ->
    if String.is_prefix ~prefix:s m
    then Some mode
    else None)

let string_to_bits s =
  Array.init
    (8 * String.length s)
    ~f:(fun i ->
      let c = Char.to_int s.[i / 8] in
      let j = i mod 8 in
      (c lsr j) land 1 = 1 )

let bits_to_string bits =
  let n = Array.length bits in
  let rec make_byte offset acc i =
    let finished = i = 8 || offset + i >= n in
    if finished then Char.of_int_exn acc
    else
      let acc = if bits.(offset + i) then acc lor (1 lsl i) else acc in
      make_byte offset acc (i + 1)
  in
  let len = (n + 7) / 8 in
  String.init len ~f:(fun i -> make_byte (8 * i) 0 0)

let parse mode s = 
  match mode with
  | Snarky -> failwith "Cannot parse snarky"
  | Ascii -> s
  | Hex -> Hex.to_string (`Hex s)
  | Binary ->
    bits_to_string
      (Array.map (String.to_array s) ~f:(function
           | '0' -> false
           | '1' -> true
           | c -> failwithf "Got non-binary character %c" c ()))

let output mode s =
  match mode with
  | Snarky ->
    String.concat_array ~sep:", "
      (Array.map (string_to_bits s) ~f:(function
           | true -> "1b"
           | false -> "0b"))
    |> sprintf "[%s]"
  | Ascii -> s
  | Hex -> let (`Hex s) = Hex.of_string s in s
  | Binary ->
    String.of_char_list
      (List.map (Array.to_list (string_to_bits s)) ~f:(function
           | true -> '1'
           | false -> '0'))

let cmd =
  let open Command.Param in
  let open Command.Let_syntax in
  let%map in_mode =
    flag "input" 
      (optional_with_default Ascii (Arg_type.create mode_of_string))
      ~doc:"Input parsing mode"
  and out_mode =
    flag "output" 
      (optional_with_default Binary (Arg_type.create mode_of_string))
      ~doc:"Output parsing mode"
  and str =
    anon ("INPUT" %: string)
  in
  fun () ->
    let h = Digestif.blake2s 32 in
    Digestif.digest_string h (parse in_mode str)
    |> Digestif.to_raw_string h
    |> output out_mode
    |> print_endline

let () =
  Command.run
    (Command.basic cmd ~summary:"Compute blake2s hashes")

