namespace Infrastructure

module QueryHandler =

  let rec private choice (queryHandlers : QueryHandler<_> list) query =
      async {
        match queryHandlers with
          | handler :: rest ->
              match! handler.Handle query with
                | NotHandled ->
                    return! choice rest query
                | Handled response ->
                    return Handled response  
                | QueryError response ->
                    return QueryError response
          | _ ->
            return NotHandled
        }

  let initialize queryHandlers : QueryHandler<_> =
    {
      Handle = choice queryHandlers
    }
