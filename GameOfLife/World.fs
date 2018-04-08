namespace GameOfLife

type CellStatus =
    | Alive
    | Dead

type Coordinate = int * int
type Width = uint32
type Height = uint32
type Dimensions = Width * Height

type World = private {
    World : Map<Coordinate, CellStatus>
    Dimensions : (Width * Height) }

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module World =

    let cellStatuses world = world.World
    let dimensions world = world.Dimensions

    ///World goes from (0,0) to (width - 1, height - 1)
    let make ((w,h) : (Width * Height)) (liveCells : Set<Coordinate>) : World option =

        let isValidWorldCoordinate ((x,y) : Coordinate) =
            not ((x < 0 || (uint32 x) >= w)
            || (y < 0 || (uint32 y) >= h))

        let cellStatus c =
            match Set.contains c liveCells with
            | true -> Alive
            | false -> Dead

        let cellsAreValid =
            liveCells
            |> Set.map isValidWorldCoordinate
            |> Set.fold (&&) true

        if not cellsAreValid then None
        else
            let world =
                [0 .. (int w)]
                |> List.map Tuple.make
                |> ListExt.apply [0 .. (int h)]
                |> List.map (fun c -> c, (cellStatus c))
                |> Map.ofList

            {World = world; Dimensions = (w,h)}
            |> Some

    let private cellStatus (world : World) (c : Coordinate) : CellStatus option =
        let data = cellStatuses world
        match Map.containsKey c data with
        | true ->
            data
            |> Map.find c
            |> Some
        | false -> None

    let private getAdjacentCells (world : World) (c : Coordinate) : Set<Coordinate> =
        let (x,y) = c
        let cells = cellStatuses world
        let directions =
            [-1; 0; 1]
            |> List.map Tuple.make
            |> ListExt.apply [-1; 0; 1]
            |> List.filter ((<>) (0,0))

        directions
        |> List.map (fun (a,b) -> (x + a, y + b))
        |> List.filter (fun k -> Map.containsKey k cells)
        |> Set.ofList

    let private tickCell (world : World) (c : Coordinate) : Coordinate * CellStatus =

        let getOutcome
            (pred : int -> bool)
            (c : Coordinate)
            (neighbours : CellStatus seq)
            =
            neighbours
            |> Seq.filter ((=) Alive)
            |> Seq.length
            |> pred
            |> (fun alive -> if alive then Alive else Dead)

        let neighbours =
            c
            |> getAdjacentCells world
            |> Seq.choose (cellStatus world)

        match cellStatus world c with
        | None -> failwith "This should never happen"
        | (Some Alive) ->
            getOutcome (fun count -> count = 2 || count = 3) c neighbours
        | (Some Dead) ->
            getOutcome ((=) 3) c neighbours
        |> (fun state -> c,state)

    let tick (world : World) : World =
        let cells = cellStatuses world
        let dims = dimensions world
 
        cells
        |> Map.toArray
        |> Array.map fst
        |> Array.Parallel.map (tickCell world)
        |> Map.ofArray
        |> (fun d -> {World = d; Dimensions = dims})