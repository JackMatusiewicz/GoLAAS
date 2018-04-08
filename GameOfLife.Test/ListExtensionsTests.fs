namespace GameOfLife.Test

open GameOfLife
open NUnit.Framework

module ListExtTests =

    [<Test>]
    let ``Given a list of functions, when they are applied to some numbers then the result is correct`` () =
        let fs = [(+) 1; (*) 5]
        let result = ListExt.apply [1;2;3] fs
        Assert.That(result, Is.EqualTo([2;3;4;5;10;15]))

    [<Test>]
    let ``Given empty list of functions, when applied then the result is the empty list`` () =
        let fs = []
        Assert.That (ListExt.apply [1..20] fs, Is.EqualTo([]))

    [<Test>]
    let ``Given empty list of values, when applied then the result is the empty list`` () =
        let fs = [(+) 1; (*) 5]
        Assert.That (ListExt.apply [] fs, Is.EqualTo([]))