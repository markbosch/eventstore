namespace Tests

module Domain =
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

  let runTests () =
    runTestsWithCLIArgs [] [||] tests |> ignore
