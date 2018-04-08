namespace GameOfLife

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ListExt =

    let apply (xs : 'x list) (fs : ('x -> 'y) list) =
        let rec loop fs (acc : 'y list list) =
            match fs with
            | [] ->
                acc
                |> List.rev
                |> List.concat
            | f::t ->
                loop t ((List.map f xs) :: acc)

        loop fs []