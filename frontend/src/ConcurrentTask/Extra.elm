module ConcurrentTask.Extra exposing (toResult)

{-| Helper module adding few functions to the ConcurrentTask module.
-}

import ConcurrentTask exposing (ConcurrentTask, map, onError, succeed)


{-| Lift a failed task into a successful one reporting the error with a `Result` type.

The main use case for this function is to get more localised errors.
Typically, your `Msg` type will only contain a single pair of `OnProgress` and `OnComplete` variants.

    type Msg
        = ...
        | OnProgress ( ConcurrentTask.Pool Msg TaskError TaskCompleted, Cmd Msg )
        | OnComplete (ConcurrentTask.Response TaskError TaskCompleted)

    type TaskCompleted
        = GotThingOne ThingOne
        | GotThingTwo ThingTwo

In the above situation, you would handle all errors in the same branch of your update function:

    case response of
        ConcurrentTask.error error -> ... -- handle all errors
        ConcurrentTask.Success ... -> ... -- handle successes

However, if instead of running `myTask`, you run `ConcurrentTask.toResult myTask`
and adjust your `TaskCompleted` type to handle results, you obtain errors that are local to the task.

    type TaskCompleted
        = GotThingOne (Result ErrorOne ThingOne)
        | GotThingTwo (Result ErrorTwo ThingTwo)

This pattern makes it easier to handle potential failures at the same place where you would handle correct answers.
It also mirrors the behavior of the elm/http library where each message variant handles its own errors.

    -- Example from elm/http
    type Msg
        = GotBook (Result Http.Error String)
        | GotItems (Result Http.Error (List String))

    -- ConcurrentTask example of handling errors lifted to a task Result.
    case response of
        ConcurrentTask.error error -> ... -- handle non-lifted errors
        ConcurrentTask.Success (GotThingOne result) ->
            -- deal with the first task result
        ConcurrentTask.Success (GotThingTwo result) ->
            -- deal with the second task result

-}
toResult : ConcurrentTask err a -> ConcurrentTask x (Result err a)
toResult task =
    map Ok task
        |> onError (\err -> succeed <| Err err)
