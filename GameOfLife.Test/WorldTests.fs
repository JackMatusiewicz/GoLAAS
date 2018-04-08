namespace GameOfLife.Test

open GameOfLife
open NUnit.Framework

module WorldTests =

    [<Test>]
    let ``Given live cells with negative coordinates, when world is create, then none is returned`` () =
        let aliveCells =
            [
                (-1, 0)
                (5,10)
                (15,20)
                (0,0)
                (20,20)
            ] |> Set.ofList
        let dimensions = (21u, 21u)

        let world = World.make dimensions aliveCells
        Assert.That(world, Is.EqualTo(None))

    [<Test>]
    let ``Given live cells with coordinates out of range, when world is create, then none is returned`` () =
        let aliveCells =
            [
                (0, 0)
                (5,10)
                (15,20)
                (0,0)
                (21,20)
            ] |> Set.ofList
        let dimensions = (21u, 21u)

        let world = World.make dimensions aliveCells
        Assert.That(world, Is.EqualTo(None))

    [<Test>]
    let ``Given live cells with valid coordinates, when world is create, then valid world is returned`` () =

        let cellsGiveExpectedResult
            (cells : Coordinate seq)
            (data : Map<Coordinate, CellStatus>)
            (expected : CellStatus)
            =
            cells
            |> Seq.map (fun k -> Map.find k data)
            |> Seq.map ((=) expected)
            |> Seq.fold (&&) true
            |> Assert.True

        let aliveCells =
            [
                (0, 0)
                (5,10)
                (15,20)
                (0,0)
                (20,20)
            ] |> Set.ofList
        let dimensions = (21u, 21u)

        let world = World.make dimensions aliveCells
        match world with
        | None ->
            Assert.Fail "Should exist"
        | Some w ->
            let dims = World.dimensions w
            let data = World.cells w
            let cells =
                data
                |> Map.toList
                |> List.map fst
                |> Set.ofList
            Assert.That(dims, Is.EqualTo((21u, 21u)))

            let aliveCells = Set.intersect aliveCells cells
            let deadCells = Set.difference aliveCells cells

            cellsGiveExpectedResult aliveCells data Alive
            cellsGiveExpectedResult deadCells data Dead

    [<Test>]
    let ``Given a world, when a tick occurs, then the blinker is updated`` () =
        let aliveCells =
            [
                (1,1)
                (2,1)
                (3,1)
            ] |> Set.ofList
        let dimensions = (4u, 4u)
        let (Some world) = World.make dimensions aliveCells
        let newWorld = World.tick world
        let updatedCells =
            World.cells newWorld
            |> Map.toList
            |> List.filter (fun (_,s) -> s = Alive)
            |> List.map fst
        Assert.That (updatedCells, Is.EqualTo([(2,0); (2,1); (2,2)]))