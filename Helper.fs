namespace Helper

  module Print =
    open Domain.Projections
    let printUl list =
        list
        |> List.iteri (fun i item -> printfn " %i %A" (i+1) item)

    let printEvents header events =
        events
        |> List.length
        |> printfn "History %s (Length: %i)" header

        events |> printUl

    let printSoldFlavour flavour state =
        state
        |> soldOfFlavours flavour
        |> printfn "Sold %A: %i" flavour
    
    let printTotalHistory history =
      history
      |> Map.fold (fun length _ events -> length + (events |> List.length)) 0
      |> printfn "Total History length: %i"
