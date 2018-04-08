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