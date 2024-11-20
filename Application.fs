namespace Application

module Api =

  open Domain

  type Query =
    | FlavourInStockOfTruck of Truck * Flavour
    | FlavourInStockOfAll of Flavour
    | FlavoursSoldOfTruck of Truck * Flavour
    | FlavoursSoldOfAll of Flavour

module QueryHandlers =

  open Api
  open Domain
  open Projections
  open Infrastructure

  // -- Queries are executed against projection, but should
  // -- be a read-model
  let flavours eventStore : QueryHandler<Query> =
    let handleQuery query  =
      match query with
      | FlavourInStockOfTruck (Truck truck,flavour) ->
        async {
          let state =
            eventStore.GetStream truck |> project flavoursInStock
              
          return
            state
            |> Map.tryFind flavour
            |> Option.defaultValue 0
            |> box
            |> Handled
        }
      
      | FlavourInStockOfAll flavour ->
        async {
          let state =
            eventStore.Get () |> Map.toList |> List.collect snd |> project flavoursInStock
          
          return
            state
            |> Map.tryFind flavour
            |> Option.defaultValue 0
            |> box
            |> Handled
        }

      | FlavoursSoldOfTruck (Truck truck, flavour) ->
        async {  
          let state =
            eventStore.GetStream truck |> project soldFlavours

          return
            state
            |> Map.tryFind flavour
            |> Option.defaultValue 0
            |> box
            |> Handled
        }
      
      | FlavoursSoldOfAll flavour ->
        async {
          let state =
            eventStore.Get () |> Map.toList |> List.collect snd |> project soldFlavours
          
          return
            state
            |> Map.tryFind flavour
            |> Option.defaultValue 0
            |> box
            |> Handled
        }

    { Handle = handleQuery }
  
