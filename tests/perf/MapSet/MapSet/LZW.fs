/// LZW data compression.
///
/// Using F# Core Map: 73% in Map.containsKey, 13% in
/// Map.add and 10% in Map.find.
///
/// Most garbage is short lived.
module LZW

// Takes ~10s.
let problemSize = 320000

/// Compress a sequence of bytes.
let compress input =
  let rec aux d n xs = function
    | [], [] -> List.rev xs |> seq
    | w, [] -> Map.find w d::xs |> List.rev |> seq
    | w, c::cs ->
        let wc = w @ [c]
        if Map.containsKey wc d then aux d n xs (wc, cs) else
          aux (Map.add wc n d) (n+1) (Map.find w d::xs) ([c], cs)
  let d = Map [ for i in 0 .. 255 -> [i], i ]
  aux d d.Count [] ([], List.ofSeq input)

/// Decompress a sequence of ints.
let decompress input =
  match List.ofSeq input with
  | [] -> []
  | x::xs ->
      let rec aux (n, d, xs, ys) x =
        let y =
          if x < 256 then [x] else
            match Map.tryFind x d with
            | Some y -> y
            | None -> xs @ [List.head xs]
        n+1, Map.add n (xs @ [List.head y]) d, y, List.rev y @ ys
      let _, _, _, ys = Seq.fold aux (256, Map.empty, [x], [x]) xs
      List.rev ys

let input = Array.init problemSize (fun n -> int(127.0+127.0*sin(float n / 10.0)))

let run() =
  let timer = System.Diagnostics.Stopwatch.StartNew()
  let output =
    input
    |> compress
    |> decompress
  let time = timer.Elapsed.TotalSeconds
  if input <> Array.ofList output then failwith "Compression/decompression error"
  time
// Real: 00:00:09.440, CPU: 00:00:09.422, GC gen0: 157, gen1: 10, gen2: 0
