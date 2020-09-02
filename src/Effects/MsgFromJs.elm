module Effects.MsgFromJs exposing
    ( Effects, subscribe, decode, succeed
    , map, decodeJsData
    )

{-| Message from JS system is used to listen for messages from JS and decode them to Elm messages.
It uses effects for communication with other systems.


# Create

@docs Effects, subscribe, decode, succeed


# Transform

@docs map, decodeJsData

-}

import Dict
import Effects as Effects
import ErrorMessage
import JsData
import Json.Decode as Decode



-- EFFECT


{-| Message from JS system effects.
-}
type alias Effects msg =
    Effects.Effects (Effect msg)


{-| Maps effects.
-}
map : (msgA -> msgB) -> Effects msgA -> Effects msgB
map tagger =
    Effects.map (mapEffect tagger)


{-| Describes available effects.
-}
type Effect msg
    = Subscribe String (List (ResultDecoder String msg))


{-| Maps an effect.
-}
mapEffect : (msgA -> msgB) -> Effect msgA -> Effect msgB
mapEffect tagger (Subscribe moduleName decoders) =
    decoders
        |> mapResultDecoders (Result.map tagger)
        |> Subscribe moduleName


{-| Subscribes to listen incoming messages from JS.
Module name is used for error logging.
-}
subscribe : String -> List (ResultDecoder Decode.Error msg) -> Effects msg
subscribe moduleName decoders =
    decoders
        |> mapResultDecoders (Result.mapError Decode.errorToString)
        |> Subscribe moduleName
        |> Effects.from



-- DECODER


{-| Represents decoder structure.
First element of a tuple is a JS message tag name, the second is a data decoder.
-}
type alias Decoder decoder =
    ( String, decoder )


{-| Maps decoders.
-}
mapDecoders : (decoderA -> decoderB) -> List (Decoder decoderA) -> List (Decoder decoderB)
mapDecoders tagger decoders =
    List.map (Tuple.mapSecond tagger) decoders


{-| Represents a decoder which takes a JSON value and returns a result of either decoding error or a decoded message.
-}
type alias ResultDecoder error msg =
    Decoder (Decode.Value -> Result error msg)


{-| Maps result decoders.
-}
mapResultDecoders : (Result errorA msgA -> Result errorB msgB) -> List (ResultDecoder errorA msgA) -> List (ResultDecoder errorB msgB)
mapResultDecoders tagger decoders =
    mapDecoders (\toResult decodeValue -> tagger (toResult decodeValue)) decoders



-- SUBSCRIPTION


{-| Decodes incoming data from JS.
It succeeds when at least one effect succeeds.
Otherwise it fails with combined error from all effects.
Effects are processed independently, means that different effects can reuse same message tag names.
-}
decodeJsData : (List msg -> msg) -> (String -> msg) -> Effects msg -> JsData.JsData -> msg
decodeJsData toMsg logError effects jsData =
    case
        Effects.apply (decodeEffectJsData jsData) (Err [ ErrorMessage.unexpectedMessageFromJs jsData ]) effects
            |> Result.map toMsg
            |> Result.mapError (logError << String.join "\n")
    of
        Ok msg ->
            msg

        Err msg ->
            msg


{-| Decodes incoming JS data for an effect.
Decoded messages are combined into a list, which means that multiple effects can succeed from the same message tag name.
Decoding errors are combined into a list, which means that we log all the places where message tag name suppose to be used.
-}
decodeEffectJsData : JsData.JsData -> Effect msg -> Result (List String) (List msg) -> Result (List String) (List msg)
decodeEffectJsData jsData (Subscribe moduleName decoders) result =
    case
        ( mapDecoders (\toResult -> toResult jsData.data) decoders
            |> Dict.fromList
            |> Dict.get jsData.tag
            |> Maybe.withDefault (Err "")
        , result
        )
    of
        ( Ok msg, Ok _ ) ->
            Result.map ((::) msg) result

        ( Ok msg, Err _ ) ->
            Ok [ msg ]

        ( Err _, Ok msgs ) ->
            Ok msgs

        ( Err error, Err _ ) ->
            if error /= "" then
                Result.mapError ((::) (moduleName ++ " module: " ++ ErrorMessage.typeMismatch error)) result

            else
                result



-- HELPER FUNCTIONS


{-| Decodes JSON value into a result.
-}
decode : (value -> msg) -> Decode.Decoder value -> Decode.Value -> Result Decode.Error msg
decode toMsg decoder decodeValue =
    Result.map toMsg (Decode.decodeValue decoder decodeValue)


{-| Succeeds with a message without decoding JSON value.
-}
succeed : msg -> Decode.Value -> Result Decode.Error msg
succeed msg _ =
    Ok msg
