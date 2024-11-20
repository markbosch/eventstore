open Infrastructure
open Helper.Print
open Domain
open Projections
open Application.Api
open Tests.Domain

runTests()

let eventStore : EventStore<Event> = EventStore.initialize()

let queryHandler =
  QueryHandler.initialize
    [
        Application.QueryHandlers.flavours eventStore
    ]

let guid (Truck truck) = truck

let truck1 = Truck <| System.Guid.NewGuid()
let truck2 = Truck <| System.Guid.NewGuid()

let truck1_guid = guid truck1
let truck2_guid = guid truck2

eventStore.Evolve truck1_guid (Behavior.sellFlavour Vanilla)
eventStore.Evolve truck1_guid (Behavior.sellFlavour Strawberry)
eventStore.Evolve truck1_guid (Behavior.restock Vanilla 3)
eventStore.Evolve truck1_guid (Behavior.sellFlavour Vanilla)

eventStore.Evolve truck2_guid (Behavior.sellFlavour Vanilla)


let eventsTruck1 = eventStore.GetStream truck1_guid
let eventsTruck2 = eventStore.GetStream truck2_guid



eventsTruck1 |> printEvents " Truck1"
eventsTruck2 |> printEvents " Truck2"


(*
    Map {
        Vanilla => 3
        Strawberry => 1
    }
*)

let sold : Map<Flavour, int> = 
    eventsTruck1 |> project soldFlavours

printSoldFlavour Vanilla sold
printSoldFlavour Strawberry sold

let stock =
    eventsTruck1 |> project flavoursInStock

// -- Queries
let queries =
  [
    ("FlavourInStockOfTruck (truck1, Vanilla)",
      fun () -> FlavourInStockOfTruck (truck1, Vanilla)
                |> queryHandler.Handle
                |> printQueryResults "Stock Truck One Vanilla")
  ]

FlavourInStockOfTruck (truck1, Vanilla)
  |> queryHandler.Handle
  |> printQueryResults "Stock Truck One Vanilla"

FlavoursSoldOfTruck (truck1, Vanilla)
  |> queryHandler.Handle
  |> printQueryResults "Flavour Vanilla sold in Truck One"

FlavourInStockOfAll (Vanilla)
  |> queryHandler.Handle
  |> printQueryResults "Stock of Vanilla in all trucks"

FlavoursSoldOfAll (Vanilla)
  |> queryHandler.Handle
  |> printQueryResults "Sold Vanilla flavour in all trucks"      
