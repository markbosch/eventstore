open Infrastructure
open Helper.Print
open Domain
open Projections
open Tests.Domain

runTests()

let eventStore : EventStore<Event> = EventStore.initialize()

let truck1 = System.Guid.NewGuid()
let truck2 = System.Guid.NewGuid()

eventStore.Evolve truck1 (Behavior.sellFlavour Vanilla)
eventStore.Evolve truck1 (Behavior.sellFlavour Strawberry)
eventStore.Evolve truck1 (Behavior.restock Vanilla 3)
eventStore.Evolve truck1 (Behavior.sellFlavour Vanilla)

eventStore.Evolve truck2 (Behavior.sellFlavour Vanilla)


let eventsTruck1 = eventStore.GetStream truck1
let eventsTruck2 = eventStore.GetStream truck2



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
