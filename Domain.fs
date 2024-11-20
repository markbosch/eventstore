namespace Domain

type Truck = Truck of System.Guid

type Flavour =
  | Strawberry
  | Vanilla

type Event =
  | FlavourSoldEvent of Flavour
  | FlavourRestockedEvent of Flavour * int  /// this is a tuple
  | FlavourWentOutOfStockEvent of Flavour
  | FlavourWasNotInStockEvent of Flavour


module Projections =

    type Projection<'State, 'Event> =
        {
            Init : 'State
            Update : 'State -> 'Event -> 'State
        }

    let project (projection : Projection<_,_>) events =
        events |> List.fold projection.Update projection.Init

    let soldOfFlavours flavour state =
        state
        |> Map.tryFind flavour
        |> Option.defaultValue 0
    
    let updateSoldFlavours state event =
        match event with
        | FlavourSoldEvent flavour ->
            state
            |> soldOfFlavours flavour
            |> fun portion -> state |> Map.add flavour (portion + 1)
        | _ -> state

    let soldFlavours : Projection<Map<Flavour, int>, Event> =
        {
            Init = Map.empty
            Update = updateSoldFlavours
        }
    
    let restock flavour number stock =
        stock
            |> Map.tryFind flavour
            |> Option.defaultValue 0
            |> fun portions -> stock |> Map.add flavour (portions + number)

    let updateFlavoursInStock stock event =
        match event with
        | FlavourSoldEvent flavour ->
            stock |> restock flavour -1

        | FlavourRestockedEvent (flavour, number) ->
            stock |> restock flavour number

        | _ -> stock

    let flavoursInStock : Projection<Map<Flavour, int>, Event> =
        {
            Init = Map.empty
            Update = updateFlavoursInStock
        }
    
    let stockOf flavour stock =
        stock
        |> Map.tryFind flavour
        |> Option.defaultValue 0

module Behavior =
    open Projections

    let sellFlavour  flavour (events : Event list) =
        let stock =
            events
            |> project flavoursInStock
            |> stockOf flavour

        match stock with
        | 0 -> [FlavourWasNotInStockEvent flavour]
        | 1 -> [FlavourSoldEvent flavour ; FlavourWentOutOfStockEvent flavour]
        | _ -> [FlavourSoldEvent flavour]
    
    let restock flavour number events =
        [FlavourRestockedEvent (flavour,number)]
