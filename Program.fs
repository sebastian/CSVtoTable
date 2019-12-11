open System
open System.IO
open ResultBuilder

let (>=>) f g = f >> Result.bind g

let split (line: string) = 
  line.Split ";" |> Array.map (fun (line: string) -> line.Trim())

let getHeaders lineSeq =
  match lineSeq |> Seq.take 1 |> Seq.tryHead with
  | Some (header: string) -> Ok (split header, Seq.skip 1 lineSeq)
  | None -> Error "The file was empty"

type Guess = 
  | Int
  | Real
  | PotentiallyBool
  | Text
  | NULL
  | UnknownOrUnexpectedlyInconsistent

type GuessClass =
  | Consistent of Guess * int
  | Inconsistent of Map<Guess, int>

type GuessType =
  | NoGuess
  | HasGuesses of GuessClass array

let (|IsNullValue|_|) (value: string) =
  if value = "" then Some NULL else None

let looksLikeBool =
  function
  | "true"
  | "yes"
  | "one"
  | "false"
  | "no"
  | "off" -> true
  | _ -> false

let (|MaybeBool|_|) (value: string) =
  match Int32.TryParse value with
  | true, value -> if value = 0 || value = 1 then Some true else None
  | false, _ -> if looksLikeBool value then Some true else None

let toMaybe =
  function
  | true, parsedValue -> Some parsedValue
  | _ -> None

let (|MaybeInt|_|) (value: string) =
   value |> Int32.TryParse |> toMaybe

let (|LikelyReal|_|) (value: string) =
  if value.Contains "," || value.Contains "." 
  then value |> Double.TryParse |> toMaybe
   else None

let (|LikelyText|_|) (value: string) =
   Some value

let guessType (value: string) =
  match value.ToLower() with
  | IsNullValue _ -> NULL
  | MaybeBool _ -> PotentiallyBool
  | LikelyReal _ -> Real
  | MaybeInt _ -> Int
  | LikelyText _ -> Text
  | _ -> UnknownOrUnexpectedlyInconsistent

let equivalenceClasses a b =
  match List.sort [a; b] with
  | [a; b] when a = b -> Some a
  | [other; NULL] -> Some other
  | [Int; Real] -> Some Real
  | [Int; PotentiallyBool] -> Some Int
  | [Real; PotentiallyBool] -> Some Real
  | _ -> None

let guessAndValidate (value, guesses) =
  let newGuess = guessType value
  match guesses with
  | Consistent (currentGuess, count) ->
    match equivalenceClasses newGuess currentGuess with
    | Some validClass -> Consistent (validClass, count + 1)
    | None -> Inconsistent <| Map.ofList [(currentGuess, count); (newGuess, 1)]
  | Inconsistent guesses ->
    let newGuess = guessType value
    guesses
    |> Map.tryFind newGuess
    |> function
      | Some count -> Map.add newGuess (count+1) guesses
      | None -> Map.add newGuess 1 guesses
    |> Inconsistent

let guessTypeForLine guesses values =
  match guesses with
  | NoGuess -> 
    values 
    |> Array.map (fun value -> Consistent (value |> guessType, 1))
    |> HasGuesses
  | HasGuesses guesses -> 
    guesses
    |> Array.zip values
    |> Array.map guessAndValidate
    |> HasGuesses

let allConsistent =
  Array.fold (fun a -> function | Consistent (guess, _) when guess <> NULL -> a | _ -> false) true

let typeToHuman =
  function
  | Int -> "int"
  | Real -> "real"
  | PotentiallyBool -> "boolean"
  | Text -> "text"
  | NULL -> failwith "Unexpectedly failed... should have weeded out null values"
  | UnknownOrUnexpectedlyInconsistent -> failwith "Unexpectedly failed... should have had consistent types"

let unlines = sprintf "%s\n%s"

let deriveTypeForError =
  function 
  | header, Consistent (guessedType, count) -> sprintf "- %s (%i/%i): %s" header count count (typeToHuman guessedType)
  | header, Inconsistent guesses ->
    let totalGuesses =
      guesses
      |> Map.toList
      |> List.sumBy snd
    let guessByType =
      guesses
      |> Map.toList
      |> List.map (fun (guessedType, count) -> sprintf "> %s (%i/%i)" (typeToHuman guessedType) count totalGuesses)
      |> List.reduce unlines
    sprintf "%s is unclear:\n%s" header guessByType

let unwrapGuesses = 
  Array.map (function
    | Consistent (guess, _) -> guess
    | Inconsistent _ -> UnknownOrUnexpectedlyInconsistent)

let guessTypes headers lineSeq =
  lineSeq 
  |> Seq.map split
  |> Seq.fold guessTypeForLine NoGuess
  |> function
    | NoGuess -> Error "Could not produce any guesses as to what the types were."
    | HasGuesses guesses ->
      if allConsistent guesses 
      then 
        guesses
        |> unwrapGuesses
        |> Ok 
      else
        guesses
        |> Array.zip headers
        |> Array.map deriveTypeForError
        |> Array.reduce unlines
        |> sprintf "Could not uniquely determine types:\n%s"
        |> Error

let tableName (path: string) =
  let filename = Path.GetFileNameWithoutExtension path
  filename.Replace (" ", "_")

let rec joinWith sep =
  function
  | [] -> ""
  | [value] -> value
  | value::rest -> sprintf "%s%s%s" value sep (joinWith sep rest)

let output tableName headersAndTypes =
  let columns =
    headersAndTypes
    |> Array.map (fun (header, colType) -> sprintf "    \"%s\" %s" header (typeToHuman colType))
    |> Array.toList
    |> joinWith ",\n"
  Ok <| sprintf """
  CREATE TABLE "%s" (
%s
  )
  """ tableName columns

let deriveCreateStatement path =
  result {
    let! headers, remainingLines = path |> File.ReadLines |> getHeaders 
    let! types = guessTypes headers remainingLines
    let tableName = tableName path
    let headersAndTypes = Array.zip headers types
    return! (output tableName headersAndTypes)
  }

let processFile path =
  printfn "- Processing file %s: " path
  match deriveCreateStatement path with
  | Ok createStatement -> 
    printfn "! SUCCESS"
    printfn "%s" createStatement
  | Error error -> 
    printfn "X ERROR: Failed to derive at CREATE TABLE statement for \"%s\"!" path
    printfn "%s" error
  printfn "\n\n"

let processArgs argv =
  match Array.tryHead argv with
  | Some path -> 
    Ok <| Path.GetFullPath path
  | None ->
    Error """
      USAGE: 
        ./TableCreator path/to/file.csv  
        ./TableCreator path/to/files
    """
  
let validateExists path =
  if File.Exists path || Directory.Exists path
  then Ok path
  else Error <| sprintf "ERROR: No file foudn at %s" path

let getPaths path =
  let attr = File.GetAttributes path
  let files =
    if attr.HasFlag(FileAttributes.Directory)
    then path |> Directory.GetFiles
    else [|path|]
  Ok files

let processPipeline = 
  processArgs 
  >=> validateExists
  >=> getPaths

[<EntryPoint>]
let main argv =
  match processPipeline argv with
  | Ok paths -> paths |> Array.iter (processFile >> ignore)
  | Error description -> printfn "%s" description

  0 // return an integer exit code
