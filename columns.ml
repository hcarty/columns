module Col = struct
  type elt_t =
    | String of string
    | Int of int
    | Float of float

  type t =
    | String of string array
    | Int of int array
    | Float of float array

  let length = function
    | String s -> Array.length s
    | Int i -> Array.length i
    | Float f -> Array.length f

  let of_strings s = String s
  let of_ints i = Int i
  let of_floats f = Float f

  let of_string s : elt_t = String s
  let of_int i : elt_t = Int i
  let of_float f : elt_t = Float f

  let of_string_array_guess a =
    try
      Int (Array.map int_of_string a)
    with
    | Failure _ -> begin
      try
        Float (Array.map float_of_string a)
      with
      | Failure _ -> begin
        String a
      end
    end

  let to_string = function
    | String _ as s -> s
    | Int i -> String (Array.map string_of_int i)
    | Float f -> String (Array.map string_of_float f)

  let to_int = function
    | String s -> Int (Array.map int_of_string s)
    | Int _ as i -> i
    | Float f -> Int (Array.map int_of_float f)

  let to_float = function
    | String s -> Float (Array.map float_of_string s)
    | Int i -> Float (Array.map float_of_int i)
    | Float _ as f -> f

  let get c i : elt_t =
    match c with
    | String a -> String (Array.get a i)
    | Int a -> Int (Array.get a i)
    | Float a -> Float (Array.get a i)

  let pp_elt fmt (e : elt_t) =
    match e with
    | String s -> Fmt.string fmt s
    | Int i -> Fmt.int fmt i
    | Float f -> Fmt.float fmt f

  let pp fmt c =
    let sep fmt () = Fmt.char fmt ' ' in
    match c with
    | String sl -> Fmt.array ~sep Fmt.string fmt sl
    | Int il -> Fmt.array ~sep Fmt.int fmt il
    | Float fl -> Fmt.array ~sep Fmt.float fmt fl
end

module Frame = struct
  module String_map = Map.Make(String)

  open Printf

  type t = {
    title : string;
    columns : string array;
    columns_map : int String_map.t;
    rows : string array;
    rows_map : int String_map.t;
    data : Col.t array;
  }

  let of_cols ?(title = "") ?columns ?rows data =
    if Array.length data = 0 then invalid_arg "of_cols";
    let columns =
      match columns with
      | Some c -> c
      | None -> Array.init (Array.length data) (sprintf "Column %d")
    in
    let columns_map =
      let columnsi = Array.mapi (fun i col -> i, col) columns in
      Array.fold_left (
        fun map (i, col) ->
          String_map.add col i map
      ) String_map.empty columnsi
    in
    let rows =
      match rows with
      | Some r -> r
      | None -> Array.init (Col.length data.(0)) (sprintf "Row %d")
    in
    let rows_map =
      let rowsi = Array.mapi (fun i row -> i, row) rows in
      Array.fold_left (
        fun map (i, row) ->
          String_map.add row i map
      ) String_map.empty rowsi
    in
    { title; columns; columns_map; rows; rows_map; data }

  let of_csv filename =
    let csv = Csv.load filename in
    let title, header, data =
      match csv with (t :: h) :: d -> t, h, d | _ -> invalid_arg "of_csv"
    in
    let n_cols = Csv.columns csv in
    let n_rows = Csv.lines csv in
    let columns = Array.of_list header in
    let rows =
      match Csv.sub 1 0 (n_rows - 1) 1 csv |> Csv.transpose with
      | [v] -> Array.of_list v
      | _ -> invalid_arg "of_csv"
    in
    let data =
      Csv.sub 1 1 (n_rows - 1) (n_cols - 1) csv
      |> Csv.transpose
    in
    let data =
      List.map (fun col -> Col.of_string_array_guess (Array.of_list col)) data
      |> Array.of_list
    in
    of_cols ~title ~columns ~rows data

  let get_column frame col =
    let col_i = String_map.find col frame.columns_map in
    frame.data.(col_i)

  let get_row frame row =
    let row_i = String_map.find row frame.rows_map in
    Array.init (Array.length frame.columns) (
      fun col_i ->
        Col.get frame.data.(col_i) row_i
    )

  let get_row_map frame row =
    let row_i = String_map.find row frame.rows_map in
    let a =
      Array.init (Array.length frame.columns) (
        fun col_i ->
          col_i, Col.get frame.data.(col_i) row_i
      )
    in
    Array.fold_left (
      fun map (i, elt) ->
        String_map.add frame.columns.(i) elt map
    ) String_map.empty a

  let geti frame (row_i, col_i) =
    let col = frame.data.(col_i) in
    Col.get col row_i

  let get frame (row, col) =
    let row_i = String_map.find row frame.rows_map in
    let col_i = String_map.find col frame.columns_map in
    geti frame (row_i, col_i)
end
