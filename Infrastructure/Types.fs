namespace Infrastructure

  type Aggregate = System.Guid

  type EventProducer<'Event> = 
    'Event list -> 'Event list

  type EventStore<'Event> =
    {
      Get       : unit      -> Map<Aggregate, 'Event list>
      GetStream : Aggregate -> 'Event list
      Append    : Aggregate -> 'Event list -> unit
      Evolve    : Aggregate -> EventProducer<'Event> -> unit
    }

  type QueryResult =
    | Handled of obj
    | NotHandled
    | QueryError of string

  type QueryHandler<'Query> =
    {
      Handle : 'Query -> Async<QueryResult>
    }
