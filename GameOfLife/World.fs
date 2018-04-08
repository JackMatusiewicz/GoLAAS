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

    let getWorld world = world.World
    let getDimensions world = world.Dimensions

    ///World goes from [0 -> width - 1] and [0 -> height - 1]
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