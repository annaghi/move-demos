module Move exposing
    ( System, create, Msg
    , Config, config
    , withContainer
    , Info, Return
    , Model
    )

{-|

@docs System, create, Msg

@docs Config, config
@docs withContainer

@docs Info, Return
@docs Model

-}

import Browser.Dom
import Browser.Events
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Task



-- TYPES


type alias DragIndex =
    Int


type alias DropIndex =
    Int


type alias DragElementId =
    String


type alias DropElementId =
    String


type alias ContainerElementId =
    String


type alias Position =
    { x : Float
    , y : Float
    }


type alias Return listId item =
    { dragListId : listId
    , dropListId : listId
    , dragIndex : DragIndex
    , dropIndex : DropIndex
    , dragItem : item
    }


type alias Dimensions =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type Direction
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
    | Top
    | Bottom
    | Left
    | Right
    | None



-- MODEL


type Model listId item
    = Model (Maybe (State listId item))


type alias State listId item =
    { dragListId : listId
    , dropListId : listId
    , dragIndex : DragIndex
    , dropIndex : DropIndex
    , dragItem : item
    , dragElementId : DragElementId
    , dropElementId : DropElementId
    , dragElement : Maybe Browser.Dom.Element
    , dropElement : Maybe Browser.Dom.Element
    , containerElement : Maybe Browser.Dom.Element
    , startPosition : Position
    , prevPosition : Position
    , currentPosition : Position
    , translateVector : Position
    }



-- SYSTEM


type alias System msg listId item =
    { model : Model listId item
    , subscriptions : Model listId item -> Sub msg
    , update : Msg listId item -> Model listId item -> ( Maybe (Return listId item), Model listId item, Cmd msg )
    , dragEvents : listId -> item -> DragIndex -> DragElementId -> List (Html.Attribute msg)
    , dropEvents : listId -> DropIndex -> DropElementId -> List (Html.Attribute msg)
    , ghostStyles : Model listId item -> List (Html.Attribute msg)
    , info : Model listId item -> Maybe (Info listId item)
    }


create : (Msg listId item -> msg) -> Config -> System msg listId item
create toMsg configuration =
    { model = Model Nothing
    , subscriptions = subscriptions toMsg
    , update = update configuration toMsg
    , dragEvents = dragEvents toMsg
    , dropEvents = dropEvents toMsg
    , ghostStyles = ghostStyles
    , info = info
    }



-- CONFIG


type Config
    = Config Options


type alias Options =
    { containerElementId : ContainerElementId }


config : Config
config =
    Config defaultOptions


defaultOptions : Options
defaultOptions =
    { containerElementId = "" }


withContainer : ContainerElementId -> Config -> Config
withContainer containerElementId (Config options) =
    Config { options | containerElementId = containerElementId }



-- INFO


type alias Info listId item =
    { dragListId : listId
    , dropListId : listId
    , dragIndex : DragIndex
    , dropIndex : DropIndex
    , dragItem : item
    , dragElement : Browser.Dom.Element
    , dropElement : Browser.Dom.Element
    }


info : Model listId item -> Maybe (Info listId item)
info (Model model) =
    Maybe.andThen
        (\state ->
            Maybe.map2
                (\dragElement dropElement ->
                    { dragListId = state.dragListId
                    , dropListId = state.dropListId
                    , dragIndex = state.dragIndex
                    , dropIndex = state.dropIndex
                    , dragItem = state.dragItem
                    , dragElement = dragElement
                    , dropElement = dropElement
                    }
                )
                state.dragElement
                state.dropElement
        )
        model



-- DECODERS


checkedPagePosition : Json.Decode.Decoder Position
checkedPagePosition =
    checkMainMouseButtonPressed pagePosition


pagePosition : Json.Decode.Decoder Position
pagePosition =
    Json.Decode.map2 Position
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


checkMainMouseButtonPressed : Json.Decode.Decoder a -> Json.Decode.Decoder a
checkMainMouseButtonPressed decoder =
    Json.Decode.field "button" Json.Decode.int
        |> Json.Decode.andThen
            (\button ->
                case button of
                    0 ->
                        decoder

                    _ ->
                        Json.Decode.fail "Event is only relevant when the main mouse button was pressed."
            )



-- UPDATE


type StateMsg listId item
    = MoveBrowser Position
    | OverDropItem listId DropIndex DropElementId
    | LeaveDropItem
    | GotDragItem (Result Browser.Dom.Error Browser.Dom.Element)
    | GotDropItem (Result Browser.Dom.Error Browser.Dom.Element)
    | GotContainer (Result Browser.Dom.Error Browser.Dom.Element)
    | Tick
    | NoOp


type Msg listId item
    = DownDragItem listId item DragIndex DragElementId Position
    | StateMsg (StateMsg listId item)
    | UpBrowser


subscriptions : (Msg listId item -> msg) -> Model listId item -> Sub msg
subscriptions toMsg (Model model) =
    if model /= Nothing then
        Sub.batch
            [ Browser.Events.onMouseMove
                (Json.Decode.map (MoveBrowser >> StateMsg >> toMsg) pagePosition)
            , Browser.Events.onMouseUp
                (Json.Decode.succeed (UpBrowser |> toMsg))
            , Browser.Events.onAnimationFrameDelta (always Tick >> StateMsg >> toMsg)
            ]

    else
        Sub.none


direction : Position -> Dimensions -> Dimensions -> Direction
direction position element viewport =
    if position.y < element.y && position.x < element.x then
        TopLeft

    else if position.y < element.y && position.x > (element.x + viewport.width) then
        TopRight

    else if position.y > (element.y + viewport.height) && position.x < element.x then
        BottomLeft

    else if position.y > (element.y + viewport.height) && position.x > (element.x + viewport.width) then
        BottomRight

    else if position.y < element.y then
        Top

    else if position.y > (element.y + viewport.height) then
        Bottom

    else if position.x < element.x then
        Left

    else if position.x > (element.x + viewport.width) then
        Right

    else
        None


positionWithFence : State listId item -> Browser.Dom.Element -> Position
positionWithFence { startPosition, currentPosition } { element } =
    if
        (startPosition.x < element.x && currentPosition.x < element.x)
            || (startPosition.x < element.x && currentPosition.y < element.y)
    then
        Position
            (currentPosition.x - startPosition.x)
            (currentPosition.y - startPosition.y)

    else
        case direction currentPosition element element of
            TopLeft ->
                Position
                    (element.x - startPosition.x)
                    (element.y - startPosition.y)

            TopRight ->
                Position
                    (element.x + element.width - startPosition.x)
                    (element.y - startPosition.y)

            BottomLeft ->
                Position
                    (element.x - startPosition.x)
                    (element.y + element.height - startPosition.y)

            BottomRight ->
                Position
                    (element.x + element.width - startPosition.x)
                    (element.y + element.height - startPosition.y)

            Top ->
                Position
                    (currentPosition.x - startPosition.x)
                    (element.y - startPosition.y)

            Bottom ->
                Position
                    (currentPosition.x - startPosition.x)
                    (element.y + element.height - startPosition.y)

            Left ->
                Position
                    (element.x - startPosition.x)
                    (currentPosition.y - startPosition.y)

            Right ->
                Position
                    (element.x + element.width - startPosition.x)
                    (currentPosition.y - startPosition.y)

            None ->
                Position
                    (currentPosition.x - startPosition.x)
                    (currentPosition.y - startPosition.y)


scrollByStep : Float -> State listId item -> ContainerElementId -> Browser.Dom.Element -> Browser.Dom.Viewport -> Task.Task Browser.Dom.Error ()
scrollByStep step { startPosition, prevPosition, currentPosition } containerElementId { element } { viewport } =
    if
        (startPosition.x < element.x && prevPosition.x < currentPosition.x && currentPosition.x < element.x)
            || (startPosition.x < element.x && prevPosition.x < currentPosition.x && currentPosition.y < element.y)
    then
        Task.succeed ()

    else
        case direction currentPosition element viewport of
            TopLeft ->
                Task.succeed ()

            TopRight ->
                Task.succeed ()

            BottomLeft ->
                Task.succeed ()

            BottomRight ->
                Task.succeed ()

            Top ->
                Browser.Dom.setViewportOf containerElementId viewport.x (viewport.y - step)

            Bottom ->
                Browser.Dom.setViewportOf containerElementId viewport.x (viewport.y + step)

            Left ->
                Browser.Dom.setViewportOf containerElementId (viewport.x - step) viewport.y

            Right ->
                Browser.Dom.setViewportOf containerElementId (viewport.x + step) viewport.y

            None ->
                Task.succeed ()


autoScrollCmd : State listId item -> ContainerElementId -> Cmd (Msg listId item)
autoScrollCmd state containerElementId =
    case state.containerElement of
        Just containerElement ->
            Browser.Dom.getViewportOf containerElementId
                |> Task.andThen (scrollByStep 35 state containerElementId containerElement)
                |> Task.attempt (always NoOp >> StateMsg)

        Nothing ->
            Cmd.none


updateState : Options -> StateMsg listId item -> State listId item -> ( Maybe (Return listId item), State listId item, Cmd (Msg listId item) )
updateState options msg state =
    case msg of
        MoveBrowser coordinates ->
            ( Nothing
            , { state | prevPosition = state.currentPosition, currentPosition = coordinates }
            , case state.dragElement of
                Nothing ->
                    Cmd.batch
                        [ Task.attempt (GotDragItem >> StateMsg) (Browser.Dom.getElement state.dragElementId)
                        , Task.attempt (GotContainer >> StateMsg) (Browser.Dom.getElement options.containerElementId)
                        ]

                _ ->
                    Cmd.none
            )

        OverDropItem dropListId dropIndex dropElementId ->
            ( Nothing
            , { state | dropListId = dropListId, dropIndex = dropIndex, dropElementId = dropElementId }
            , Task.attempt (GotDropItem >> StateMsg) (Browser.Dom.getElement dropElementId)
            )

        LeaveDropItem ->
            ( Nothing
            , { state | dropListId = state.dragListId, dropIndex = state.dragIndex }
            , Cmd.none
            )

        GotDragItem (Err _) ->
            ( Nothing
            , state
            , Cmd.none
            )

        GotDragItem (Ok dragElement) ->
            ( Nothing
            , { state | dragElement = Just dragElement, dropElement = Just dragElement }
            , Cmd.none
            )

        GotDropItem (Err _) ->
            ( Nothing
            , state
            , Cmd.none
            )

        GotDropItem (Ok dropElement) ->
            ( Nothing
            , { state | dropElement = Just dropElement }
            , Cmd.none
            )

        GotContainer (Err _) ->
            ( Nothing
            , state
            , Cmd.none
            )

        GotContainer (Ok containerElement) ->
            ( Nothing
            , { state | containerElement = Just containerElement }
            , Cmd.none
            )

        Tick ->
            ( Nothing
            , { state
                | translateVector =
                    case state.containerElement of
                        Just containerElement ->
                            positionWithFence state containerElement

                        Nothing ->
                            Position
                                (state.currentPosition.x - state.startPosition.x)
                                (state.currentPosition.y - state.startPosition.y)
              }
            , autoScrollCmd state options.containerElementId
            )

        NoOp ->
            ( Nothing
            , state
            , Cmd.none
            )


update : Config -> (Msg listId item -> msg) -> Msg listId item -> Model listId item -> ( Maybe (Return listId item), Model listId item, Cmd msg )
update (Config options) toMsg msg (Model model) =
    case ( msg, model ) of
        ( DownDragItem dragListId dragItem dragIndex dragElementId coordinates, _ ) ->
            ( Nothing
            , Model <|
                Just
                    { dragListId = dragListId
                    , dropListId = dragListId
                    , dragIndex = dragIndex
                    , dropIndex = dragIndex
                    , dragItem = dragItem
                    , dragElementId = dragElementId
                    , dropElementId = dragElementId
                    , dragElement = Nothing
                    , dropElement = Nothing
                    , containerElement = Nothing
                    , startPosition = coordinates
                    , prevPosition = coordinates
                    , currentPosition = coordinates
                    , translateVector = Position 0 0
                    }
            , Cmd.none
            )

        ( StateMsg stateMsg, Just state ) ->
            let
                ( return, newModel, cmds ) =
                    updateState options stateMsg state
            in
            ( return
            , Model (Just newModel)
            , Cmd.map toMsg cmds
            )

        ( UpBrowser, Just state ) ->
            if
                (state.dragListId == state.dropListId && state.dragIndex /= state.dropIndex)
                    || (state.dragListId /= state.dropListId)
            then
                ( Just
                    { dragListId = state.dragListId
                    , dropListId = state.dropListId
                    , dragIndex = state.dragIndex
                    , dropIndex = state.dropIndex
                    , dragItem = state.dragItem
                    }
                , Model Nothing
                , Cmd.none
                )

            else
                ( Nothing
                , Model Nothing
                , Cmd.none
                )

        _ ->
            ( Nothing
            , Model Nothing
            , Cmd.none
            )



-- EVENTS


dragEvents : (Msg listId item -> msg) -> listId -> item -> DragIndex -> DragElementId -> List (Html.Attribute msg)
dragEvents toMsg dragListId dragItem dragIndex dragElementId =
    [ Html.Events.preventDefaultOn "mousedown"
        (checkedPagePosition
            |> Json.Decode.map (DownDragItem dragListId dragItem dragIndex dragElementId >> toMsg)
            |> Json.Decode.map (\msg -> ( msg, True ))
        )
    ]


dropEvents : (Msg listId item -> msg) -> listId -> DropIndex -> DropElementId -> List (Html.Attribute msg)
dropEvents toMsg dropListId dropIndex dropElementId =
    [ Html.Events.onMouseOver (OverDropItem dropListId dropIndex dropElementId |> StateMsg |> toMsg)
    , Html.Events.onMouseLeave (LeaveDropItem |> StateMsg |> toMsg)
    ]



-- STYLES


px : Int -> String
px n =
    String.fromInt n ++ "px"


translate : Int -> Int -> String
translate x y =
    "translate3d(" ++ px x ++ ", " ++ px y ++ ", 0)"


styles : Position -> Browser.Dom.Element -> Html.Attribute msg
styles { x, y } { element } =
    Html.Attributes.style "transform" <|
        translate
            (round (x + element.x))
            (round (y + element.y))


baseStyles : Browser.Dom.Element -> List (Html.Attribute msg)
baseStyles { element } =
    [ Html.Attributes.style "pointer-events" "none"
    , Html.Attributes.style "height" <| px (round element.height)
    , Html.Attributes.style "width" <| px (round element.width)
    ]


ghostStyles : Model listId item -> List (Html.Attribute msg)
ghostStyles (Model model) =
    case model of
        Just state ->
            case state.dragElement of
                Just dragElement ->
                    styles state.translateVector dragElement :: baseStyles dragElement

                Nothing ->
                    []

        Nothing ->
            []
