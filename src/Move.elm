module Move exposing
    ( System, create, Msg
    , Config, config
    , withContainer, withOffset
    , Info, Return
    , Model, ListModel, GhostModel
    )

{-|

@docs System, create, Msg

@docs Config, config
@docs withContainer, withOffset

@docs Info, Return
@docs Model, ListModel, GhostModel

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


type alias Offset =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }


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


type alias Model listId item =
    { list : ListModel listId item
    , ghost : GhostModel
    }


type ListModel listId item
    = ListModel (Maybe (ListState listId item))


type alias ListState listId item =
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
    }


type GhostModel
    = GhostModel (Maybe GhostState)


type alias GhostState =
    { startPosition : Position
    , prevPosition : Position
    , currentPosition : Position
    , translateVector : Position
    }



-- SYSTEM


type alias System msg listId item =
    { model : Model listId item
    , subscriptions : Model listId item -> Sub msg
    , update : Msg listId item -> Model listId item -> ( Maybe (Return listId item), ( ListModel listId item, GhostModel ), Cmd msg )
    , dragEvents : listId -> item -> DragIndex -> DragElementId -> List (Html.Attribute msg)
    , dropEvents : listId -> DropIndex -> DropElementId -> List (Html.Attribute msg)
    , ghostStyles : Model listId item -> List (Html.Attribute msg)
    , info : ListModel listId item -> Maybe (Info listId item)
    }


create : (Msg listId item -> msg) -> Config -> System msg listId item
create toMsg configuration =
    { model = Model (ListModel Nothing) (GhostModel Nothing)
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
    { containerElementId : ContainerElementId
    , offset : Offset
    }


config : Config
config =
    Config defaultOptions


defaultOffset : Offset
defaultOffset =
    { top = 0, right = 0, bottom = 0, left = 0 }


defaultOptions : Options
defaultOptions =
    { containerElementId = ""
    , offset = defaultOffset
    }


withContainer : ContainerElementId -> Config -> Config
withContainer containerElementId (Config options) =
    Config { options | containerElementId = containerElementId }


withOffset : Offset -> Config -> Config
withOffset offset (Config options) =
    Config { options | offset = offset }



-- INFO


type alias Info listId item =
    { dragListId : listId
    , dropListId : listId
    , dragIndex : DragIndex
    , dropIndex : DropIndex
    , dragItem : item
    }


info : ListModel listId item -> Maybe (Info listId item)
info (ListModel listModel) =
    case listModel of
        Just listState ->
            if listState.dragElement /= Nothing && listState.dropElement /= Nothing then
                Just
                    { dragListId = listState.dragListId
                    , dropListId = listState.dropListId
                    , dragIndex = listState.dragIndex
                    , dropIndex = listState.dropIndex
                    , dragItem = listState.dragItem
                    }

            else
                Nothing

        _ ->
            Nothing



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
subscriptions toMsg { list, ghost } =
    case ( list, ghost ) of
        ( ListModel (Just _), GhostModel (Just _) ) ->
            Sub.batch
                [ Browser.Events.onMouseMove
                    (Json.Decode.map (MoveBrowser >> StateMsg >> toMsg) pagePosition)
                , Browser.Events.onMouseUp
                    (Json.Decode.succeed (UpBrowser |> toMsg))
                , Browser.Events.onAnimationFrameDelta (always Tick >> StateMsg >> toMsg)
                ]

        _ ->
            Sub.none


direction : Position -> Offset -> Dimensions -> Dimensions -> Direction
direction position offset element viewport =
    if position.x < element.x + offset.left && position.y < element.y + offset.top then
        TopLeft

    else if position.x > element.x + viewport.width - offset.right && position.y < element.y + offset.top then
        TopRight

    else if position.x < element.x + offset.left && position.y > element.y + viewport.height - offset.bottom then
        BottomLeft

    else if position.x > element.x + viewport.width - offset.right && position.y > element.y + viewport.height - offset.bottom then
        BottomRight

    else if position.y < element.y + offset.top then
        Top

    else if position.y > element.y + viewport.height - offset.bottom then
        Bottom

    else if position.x < element.x + offset.left then
        Left

    else if position.x > element.x + viewport.width - offset.right then
        Right

    else
        None


positionWithFence : GhostState -> Offset -> Browser.Dom.Element -> Position
positionWithFence { startPosition, currentPosition } offset { element } =
    if
        (startPosition.x < element.x && currentPosition.x < element.x)
            || (startPosition.x < element.x && currentPosition.y < element.y)
    then
        Position
            (currentPosition.x - startPosition.x)
            (currentPosition.y - startPosition.y)

    else
        case direction currentPosition offset element element of
            TopLeft ->
                Position
                    (element.x - startPosition.x + offset.left)
                    (element.y - startPosition.y + offset.top)

            TopRight ->
                Position
                    (element.x + element.width - startPosition.x - offset.right)
                    (element.y - startPosition.y + offset.top)

            BottomLeft ->
                Position
                    (element.x - startPosition.x + offset.left)
                    (element.y + element.height - startPosition.y - offset.bottom)

            BottomRight ->
                Position
                    (element.x + element.width - startPosition.x - offset.right)
                    (element.y + element.height - startPosition.y - offset.bottom)

            Top ->
                Position
                    (currentPosition.x - startPosition.x)
                    (element.y - startPosition.y + offset.top)

            Bottom ->
                Position
                    (currentPosition.x - startPosition.x)
                    (element.y + element.height - startPosition.y - offset.bottom)

            Left ->
                Position
                    (element.x - startPosition.x + offset.left)
                    (currentPosition.y - startPosition.y)

            Right ->
                Position
                    (element.x + element.width - startPosition.x - offset.right)
                    (currentPosition.y - startPosition.y)

            None ->
                Position
                    (currentPosition.x - startPosition.x)
                    (currentPosition.y - startPosition.y)


scrollByStep : Float -> GhostState -> Offset -> ContainerElementId -> Browser.Dom.Element -> Browser.Dom.Viewport -> Task.Task Browser.Dom.Error ()
scrollByStep step { startPosition, prevPosition, currentPosition } offset containerElementId { element } { viewport } =
    if
        (startPosition.x < element.x && prevPosition.x < currentPosition.x && currentPosition.x < element.x)
            || (startPosition.x < element.x && prevPosition.x < currentPosition.x && currentPosition.y < element.y)
    then
        Task.succeed ()

    else
        case direction currentPosition offset element viewport of
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


autoScrollCmd : ListState listId item -> GhostState -> Options -> Cmd (Msg listId item)
autoScrollCmd listState ghostState { containerElementId, offset } =
    case listState.containerElement of
        Just containerElement ->
            Browser.Dom.getViewportOf containerElementId
                |> Task.andThen (scrollByStep 35 ghostState offset containerElementId containerElement)
                |> Task.attempt (always NoOp >> StateMsg)

        Nothing ->
            Cmd.none


updateState : Options -> StateMsg listId item -> ListState listId item -> GhostState -> ( Maybe (Return listId item), ( ListState listId item, GhostState ), Cmd (Msg listId item) )
updateState options msg listState ghostState =
    case msg of
        MoveBrowser coordinates ->
            ( Nothing
            , ( listState, { ghostState | prevPosition = ghostState.currentPosition, currentPosition = coordinates } )
            , case listState.dragElement of
                Nothing ->
                    Cmd.batch
                        [ Task.attempt (GotDragItem >> StateMsg) (Browser.Dom.getElement listState.dragElementId)
                        , Task.attempt (GotContainer >> StateMsg) (Browser.Dom.getElement options.containerElementId)
                        ]

                _ ->
                    Cmd.none
            )

        OverDropItem dropListId dropIndex dropElementId ->
            ( Nothing
            , ( { listState | dropListId = dropListId, dropIndex = dropIndex, dropElementId = dropElementId }, ghostState )
            , Task.attempt (GotDropItem >> StateMsg) (Browser.Dom.getElement dropElementId)
            )

        LeaveDropItem ->
            ( Nothing
            , ( { listState | dropListId = listState.dragListId, dropIndex = listState.dragIndex }, ghostState )
            , Cmd.none
            )

        GotDragItem (Err _) ->
            ( Nothing
            , ( listState, ghostState )
            , Cmd.none
            )

        GotDragItem (Ok dragElement) ->
            ( Nothing
            , ( { listState | dragElement = Just dragElement, dropElement = Just dragElement }, ghostState )
            , Cmd.none
            )

        GotDropItem (Err _) ->
            ( Nothing
            , ( listState, ghostState )
            , Cmd.none
            )

        GotDropItem (Ok dropElement) ->
            ( Nothing
            , ( { listState | dropElement = Just dropElement }, ghostState )
            , Cmd.none
            )

        GotContainer (Err _) ->
            ( Nothing
            , ( listState, ghostState )
            , Cmd.none
            )

        GotContainer (Ok containerElement) ->
            ( Nothing
            , ( { listState | containerElement = Just containerElement }, ghostState )
            , Cmd.none
            )

        Tick ->
            ( Nothing
            , ( listState
              , { ghostState
                    | translateVector =
                        case listState.containerElement of
                            Just containerElement ->
                                positionWithFence ghostState options.offset containerElement

                            Nothing ->
                                Position
                                    (ghostState.currentPosition.x - ghostState.startPosition.x)
                                    (ghostState.currentPosition.y - ghostState.startPosition.y)
                }
              )
            , autoScrollCmd listState ghostState options
            )

        NoOp ->
            ( Nothing
            , ( listState, ghostState )
            , Cmd.none
            )


update : Config -> (Msg listId item -> msg) -> Msg listId item -> Model listId item -> ( Maybe (Return listId item), ( ListModel listId item, GhostModel ), Cmd msg )
update (Config options) toMsg msg { list, ghost } =
    case ( msg, list, ghost ) of
        ( DownDragItem dragListId dragItem dragIndex dragElementId coordinates, _, _ ) ->
            ( Nothing
            , ( ListModel <|
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
                        }
              , GhostModel <|
                    Just
                        { startPosition = coordinates
                        , prevPosition = coordinates
                        , currentPosition = coordinates
                        , translateVector = Position 0 0
                        }
              )
            , Cmd.none
            )

        ( StateMsg stateMsg, ListModel (Just listState), GhostModel (Just ghostState) ) ->
            let
                ( return, ( newListState, newGhostState ), cmds ) =
                    updateState options stateMsg listState ghostState
            in
            ( return
            , ( ListModel (Just newListState), GhostModel (Just newGhostState) )
            , Cmd.map toMsg cmds
            )

        ( UpBrowser, ListModel (Just listState), GhostModel (Just _) ) ->
            if
                (listState.dragListId == listState.dropListId && listState.dragIndex /= listState.dropIndex)
                    || (listState.dragListId /= listState.dropListId)
            then
                ( Just
                    { dragListId = listState.dragListId
                    , dropListId = listState.dropListId
                    , dragIndex = listState.dragIndex
                    , dropIndex = listState.dropIndex
                    , dragItem = listState.dragItem
                    }
                , ( ListModel Nothing, GhostModel Nothing )
                , Cmd.none
                )

            else
                ( Nothing
                , ( ListModel Nothing, GhostModel Nothing )
                , Cmd.none
                )

        _ ->
            ( Nothing
            , ( ListModel Nothing, GhostModel Nothing )
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
ghostStyles { list, ghost } =
    case ( list, ghost ) of
        ( ListModel (Just listState), GhostModel (Just ghostState) ) ->
            case listState.dragElement of
                Just dragElement ->
                    styles ghostState.translateVector dragElement :: baseStyles dragElement

                Nothing ->
                    []

        _ ->
            []
