namespace Helper

  module Print =
    open System
    open AsyncPrimitives
    open Domain.Projections

    let waitForAnyKey () =
      Console.WriteLine "\n\nPress any key to continue.\n\n"
      Console.ReadKey() |> ignore
    
    let runAsync asnc =
      asnc |> Async.RunSynchronously

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
    
    let printQueryResults header result =
      result
      |> runAsync
      |> function
        | Infrastructure.QueryResult.Handled result ->
            printfn "\n%s: %A" header result
        
        | Infrastructure.QueryResult.NotHandled ->
            printfn "\n%s: NOT HANDLED" header
        
        | Infrastructure.QueryResult.QueryError error ->
            printfn "Query error: %s" error
      
      waitForAnyKey()
