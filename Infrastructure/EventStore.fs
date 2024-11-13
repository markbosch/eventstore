namespace Infrastructure

module EventStore =

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
