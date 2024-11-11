module EventStore =

    type EventProducer<'Event> =
        'Event list -> 'Event list
    
    type Aggregate = System.Guid

    type EventStore<'Event> =
        {
            Get       : unit -> Map<Aggregate, 'Event list>
            GetStream : Aggregate -> 'Event list
            Append    : Aggregate -> 'Event list -> unit
            Evolve    : Aggregate -> EventProducer<'Event> -> unit
        }

    type Msg<'Event> =
        | Append of Aggregate * 'Event list
        | GetStream of Aggregate * AsyncReplyChannel<'Event list>
        | Get of AsyncReplyChannel<Map<Aggregate, 'Event list>>
        | Evolve of Aggregate * EventProducer<'Event>

    let eventsForAggregate aggregate history =
        history
        |> Map.tryFind aggregate
        |> Option.defaultValue []

    let initialize () : EventStore<'Event> =
        let agent =
            MailboxProcessor.Start(fun inbox ->
                let rec loop history =
                    async {
                        let! msg = inbox.Receive()  /// let! == await

                        match msg with
                        | Append (aggregate, events) ->
                            let streamEvents =
                                history |> eventsForAggregate aggregate
                            
                            let newHistory =
                                history
                                |> Map.add aggregate (streamEvents @ events)
                            return! loop newHistory

                        | Get reply ->
                            reply.Reply history
                            return! loop history
                        
                        | GetStream (aggregate, reply) ->
                            reply.Reply (history |> eventsForAggregate aggregate)

                            return! loop history

                        | Evolve (aggregate, eventProducer)  ->
                            let streamEvents =
                                history |> eventsForAggregate aggregate
                            
                            let newEvents =
                                eventProducer streamEvents
                            
                            let newHistory =
                                history
                                |> Map.add aggregate (streamEvents @ newEvents)
                            
                            return! loop newHistory
                    }

                loop Map.empty
            )

        let append aggregate events =
            agent.Post (Append (aggregate, events))

        let get () =
            agent.PostAndReply Get
        
        let getStream aggregate =
            agent.PostAndReply (fun reply -> GetStream (aggregate,reply))

        let evolve aggregate eventProducer =
            agent.Post (Evolve (aggregate, eventProducer))

        {
            Get       = get
            GetStream = getStream
            Append    = append
            Evolve    = evolve
        }

module Domain =

    type Flavour =
        | Strawberry
        | Vanilla

    type Event =
        | FlavourSoldEvent of Flavour
        | FlavourRestockedEvent of Flavour * int  /// this is a tuple
        | FlavourWentOutOfStockEvent of Flavour
        | FlavourWasNotInStockEvent of Flavour


module Projections =
    open Domain
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
    open Domain
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

module Helper =
    open Projections

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


module Tests =
    open Expecto
    open Expecto.Expect
    open Domain

    /// -- Small DSL
    /// Given
    /// When
    /// Then
    let Given =
        id
    
    let When eventProducer events =
        eventProducer events
    
    let Then expectedEvents actualEvents =
        equal actualEvents expectedEvents "actual events should equal expected events"

    let tests =
        testList "Sell Flavour"
            [
               test "FlavourSoldEvent happy path" {
                    Given [ FlavourRestockedEvent(Vanilla, 3) ]
                    |> When (Behavior.sellFlavour Vanilla)
                    |> Then [ FlavourSoldEvent Vanilla ]
               }

               test "FlavourSold, Flavour went out of Stock" {
                    Given [ FlavourRestockedEvent(Vanilla, 1) ]
                    |> When (Behavior.sellFlavour Vanilla)
                    |> Then [ FlavourSoldEvent Vanilla ; FlavourWentOutOfStockEvent Vanilla ]
               }
               test "Flavour was not in stock" {
                    Given
                        [
                            FlavourRestockedEvent(Vanilla, 3)
                            FlavourSoldEvent Vanilla
                            FlavourSoldEvent Vanilla
                            FlavourSoldEvent Vanilla
                        ]
                    |> When (Behavior.sellFlavour Vanilla)
                    |> Then [ FlavourWasNotInStockEvent Vanilla ]
               }
            ]

open EventStore
open Helper
open Domain
open Projections
open Expecto

let runTests () =
  runTestsWithCLIArgs [] [||] Tests.tests |> ignore


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
