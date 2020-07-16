module Table exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html
import Html.Attributes
import Html.Events
import Html.Keyed
import Html.Lazy
import Json.Decode
import Move
import Task
import WeakCss


type alias Key =
    Int


total : Key
total =
    0


isTotal : Int -> Bool
isTotal index =
    index == 0


numberOfSafetyMarginRows : Int
numberOfSafetyMarginRows =
    0


numberOfSafetyMarginCols : Int
numberOfSafetyMarginCols =
    0


cornerCellId : String
cornerCellId =
    "id-corner-cell"


innerCellId : String
innerCellId =
    "id-inner-cell"


scrollableContainerId : String
scrollableContainerId =
    "id-container-scroll"



-- DND


type MovableList
    = Rows
    | Cols


dnd : Move.System Msg MovableList Key
dnd =
    Move.config
        |> Move.withContainer scrollableContainerId
        |> Move.create DnDMsg



-- MODEL


type alias SampleElements =
    { container : Browser.Dom.Element
    , cornerCell : Browser.Dom.Element
    , innerCell : Browser.Dom.Element
    }


type alias Window =
    { startRowIndex : Int
    , startColIndex : Int
    , numberOfVisibleRows : Int
    , numberOfVisibleCols : Int
    }


type alias Model =
    { dndModel : Move.Model MovableList Key
    , rows : List Key
    , cols : List Key
    , sampleElements : Maybe SampleElements
    , window : Window
    }


init : ( Model, Cmd Msg )
init =
    ( { dndModel = dnd.model
      , rows = List.range 1 2000
      , cols = List.range 2001 5000
      , sampleElements = Nothing
      , window =
            { startRowIndex = 0
            , startColIndex = 0
            , numberOfVisibleRows = 0
            , numberOfVisibleCols = 0
            }
      }
    , Cmd.none
    )



-- CONVERSIONS


type alias HeaderClass =
    String


movableListToHeaderClass : MovableList -> HeaderClass
movableListToHeaderClass listId =
    case listId of
        Rows ->
            "rows"

        Cols ->
            "cols"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dnd.subscriptions model.dndModel
        , if model.sampleElements == Nothing then
            Browser.Events.onAnimationFrameDelta (\_ -> MeasureElements)

          else
            Sub.none
        ]



-- UPDATE


type Msg
    = DnDMsg (Move.Msg MovableList Key)
    | MeasureElements
    | MeasuredElements (Result Browser.Dom.Error SampleElements)
    | GotContainerViewport (Result Browser.Dom.Error Browser.Dom.Viewport)
    | OnScroll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DnDMsg dndMsg ->
            let
                ( return, ( dndListModel, dndGhostModel ), dndCmd ) =
                    dnd.update dndMsg model.dndModel

                updateDnDModelLazily : Move.Model MovableList Key -> Move.Model MovableList Key
                updateDnDModelLazily dndModel =
                    if dndModel.list == dndListModel then
                        { dndModel | ghost = dndGhostModel }

                    else
                        Move.Model dndListModel dndGhostModel
            in
            case return of
                Just return_ ->
                    ( move return_ { model | dndModel = Move.Model dndListModel dndGhostModel }
                    , dndCmd
                    )

                Nothing ->
                    ( { model | dndModel = updateDnDModelLazily model.dndModel }
                    , dndCmd
                    )

        MeasureElements ->
            ( model
            , Task.map3 SampleElements
                (Browser.Dom.getElement scrollableContainerId)
                (Browser.Dom.getElement cornerCellId)
                (Browser.Dom.getElement innerCellId)
                |> Task.attempt MeasuredElements
            )

        MeasuredElements (Ok result) ->
            if model.sampleElements == Nothing then
                let
                    calculate : Window -> Window
                    calculate window =
                        { window
                            | numberOfVisibleRows = ceiling (result.container.element.height / result.innerCell.element.height) + numberOfSafetyMarginRows
                            , numberOfVisibleCols = ceiling (result.container.element.width / result.innerCell.element.width) + numberOfSafetyMarginCols
                        }
                in
                ( { model
                    | sampleElements = Just result
                    , window = calculate model.window
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        MeasuredElements (Err _) ->
            ( model, Cmd.none )

        OnScroll ->
            ( model, Task.attempt GotContainerViewport (Browser.Dom.getViewportOf scrollableContainerId) )

        GotContainerViewport (Ok viewport) ->
            let
                calculateStartIndices : Maybe SampleElements -> Window -> Window
                calculateStartIndices maybeSampleElements window =
                    case maybeSampleElements of
                        Just { innerCell } ->
                            { window
                                | startRowIndex = floor (viewport.viewport.y / innerCell.element.height)
                                , startColIndex = floor (viewport.viewport.x / innerCell.element.width)
                            }

                        _ ->
                            window
            in
            ( { model | window = calculateStartIndices model.sampleElements model.window }, Cmd.none )

        GotContainerViewport (Err _) ->
            ( model, Cmd.none )



-- YOUR CUSTOM MOVE


insertAt : Int -> item -> List item -> List item
insertAt index item list =
    List.take index list ++ (item :: List.drop index list)


reorder : Key -> Int -> Int -> List Key -> List Key
reorder dragItem dragIndex dropIndex list =
    if dragIndex < dropIndex then
        list |> List.filter ((/=) dragItem) |> insertAt (dropIndex - 1) dragItem

    else if dropIndex < dragIndex then
        list |> List.filter ((/=) dragItem) |> insertAt dropIndex dragItem

    else
        list


move : Move.Return MovableList Key -> Model -> Model
move { dragListId, dragIndex, dragItem, dropListId, dropIndex } model =
    case ( dragListId, dropListId ) of
        ( Rows, Rows ) ->
            { model
                | rows = reorder dragItem dragIndex dropIndex model.rows
            }

        ( Cols, Cols ) ->
            { model
                | cols = reorder dragItem dragIndex dropIndex model.cols
            }

        ( Rows, Cols ) ->
            { model
                | rows = List.filter ((/=) dragItem) model.rows
                , cols = insertAt dropIndex dragItem model.cols
            }

        ( Cols, Rows ) ->
            { model
                | rows = insertAt dropIndex dragItem model.rows
                , cols = List.filter ((/=) dragItem) model.cols
            }



-- VIEW


moduleClass : WeakCss.ClassName
moduleClass =
    WeakCss.namespace "t"


headerView : HeaderClass -> List ( String, Bool ) -> List (Html.Attribute Msg) -> Key -> String -> Html.Html Msg
headerView headerClass states events item htmlId =
    Html.li
        ([ moduleClass
            |> WeakCss.addMany [ "table", headerClass, "shell", "item" ]
            |> WeakCss.withStates states
         , Html.Attributes.id htmlId
         ]
            ++ events
        )
        [ Html.text <| String.fromInt item ]


keyedHeaderView : MovableList -> HeaderClass -> Move.ListModel MovableList Key -> Window -> Int -> Key -> ( String, Html.Html Msg )
keyedHeaderView listId headerClass dndListModel window index item =
    let
        htmlId : String
        htmlId =
            case ( listId, item ) of
                ( Rows, 0 ) ->
                    "row-0"

                ( Cols, 0 ) ->
                    "col-0"

                _ ->
                    (String.fromInt >> (++) "id-") item

        globalIndex : Int
        globalIndex =
            case listId of
                Rows ->
                    window.startRowIndex + index

                Cols ->
                    window.startColIndex + index

        states : List ( String, Bool )
        states =
            case dnd.info dndListModel of
                Just { dragListId, dropListId, dragIndex, dropIndex, dragItem } ->
                    [ ( "total", isTotal globalIndex )
                    , ( "placeholder", dragIndex == globalIndex && dragListId == listId )
                    , ( "mouseover", dropIndex == globalIndex && dropListId == listId && dragItem /= item )
                    ]

                _ ->
                    [ ( "total", isTotal globalIndex ) ]

        events : List (Html.Attribute Msg)
        events =
            if dnd.info dndListModel == Nothing && not (isTotal globalIndex) then
                dnd.dragEvents listId item globalIndex htmlId

            else
                dnd.dropEvents listId globalIndex htmlId
    in
    ( htmlId, headerView headerClass states events item htmlId )


rowHeadersView : MovableList -> Move.ListModel MovableList Key -> List Key -> Window -> Maybe SampleElements -> Html.Html Msg
rowHeadersView listId dndListModel list window maybeSampleElements =
    let
        headerClass : HeaderClass
        headerClass =
            movableListToHeaderClass listId

        height =
            case maybeSampleElements of
                Just { innerCell } ->
                    innerCell.element.height

                _ ->
                    0
    in
    Html.div
        [ moduleClass |> WeakCss.nestMany [ "table", headerClass ] ]
        [ (total :: list)
            |> List.drop window.startRowIndex
            |> List.take window.numberOfVisibleRows
            |> List.indexedMap (keyedHeaderView listId headerClass dndListModel window)
            |> Html.Keyed.node "ul"
                [ moduleClass |> WeakCss.nestMany [ "table", headerClass, "shell" ]
                , Html.Attributes.style "top" <| String.fromInt (round (toFloat window.startRowIndex * height)) ++ "px"
                ]
        ]


colHeadersView : MovableList -> Move.ListModel MovableList Key -> List Key -> Window -> Maybe SampleElements -> Html.Html Msg
colHeadersView listId dndListModel list window maybeSampleElements =
    let
        headerClass : HeaderClass
        headerClass =
            movableListToHeaderClass listId

        width =
            case maybeSampleElements of
                Just { innerCell } ->
                    innerCell.element.width

                _ ->
                    0
    in
    Html.div
        [ moduleClass |> WeakCss.nestMany [ "table", headerClass ] ]
        [ (total :: list)
            |> List.drop window.startColIndex
            |> List.take window.numberOfVisibleCols
            |> List.indexedMap (keyedHeaderView listId headerClass dndListModel window)
            |> Html.Keyed.node "ul"
                [ moduleClass |> WeakCss.nestMany [ "table", headerClass, "shell" ]
                , Html.Attributes.style "left" <| String.fromInt (round (toFloat window.startColIndex * width)) ++ "px"
                ]
        ]


cellView : Key -> Key -> Html.Html Msg
cellView row col =
    Html.li
        [ moduleClass
            |> WeakCss.addMany [ "table", "cells", "row", "item" ]
            |> WeakCss.withStates [ ( "total", row == total || col == total ) ]
        ]
        [ Html.text (String.fromInt row ++ " · " ++ String.fromInt col) ]


cellsView : List Key -> List Key -> Window -> Maybe SampleElements -> Html.Html Msg
cellsView rows cols window maybeSampleElements =
    let
        ( width, height ) =
            case maybeSampleElements of
                Just { innerCell } ->
                    ( innerCell.element.width, innerCell.element.height )

                _ ->
                    ( 0, 0 )
    in
    (total :: rows)
        |> List.drop window.startRowIndex
        |> List.take window.numberOfVisibleRows
        |> List.map
            (\row ->
                (total :: cols)
                    |> List.drop window.startColIndex
                    |> List.take window.numberOfVisibleCols
                    |> List.map (\col -> cellView row col)
                    |> Html.ul [ moduleClass |> WeakCss.nestMany [ "table", "cells", "row" ] ]
            )
        |> Html.div
            [ moduleClass |> WeakCss.nestMany [ "table", "cells" ]
            , Html.Attributes.style "top" <| String.fromInt (round (toFloat window.startRowIndex * height)) ++ "px"
            , Html.Attributes.style "left" <| String.fromInt (round (toFloat window.startColIndex * width)) ++ "px"
            ]


tableView : Model -> Html.Html Msg
tableView model =
    Html.div
        [ moduleClass |> WeakCss.nest "table" ]
        [ Html.Lazy.lazy4 cellsView model.rows model.cols model.window model.sampleElements
        , Html.div [ moduleClass |> WeakCss.nestMany [ "table", "corner" ] ] [ Html.text "corner" ]
        , Html.Lazy.lazy5 rowHeadersView Rows model.dndModel.list model.rows model.window model.sampleElements
        , Html.Lazy.lazy5 colHeadersView Cols model.dndModel.list model.cols model.window model.sampleElements
        ]


ghostView : Move.Model MovableList Key -> Html.Html Msg
ghostView dndModel =
    case dnd.info dndModel.list of
        Just { dragItem } ->
            Html.div
                ((moduleClass |> WeakCss.nest "ghost") :: dnd.ghostStyles dndModel)
                [ Html.text <| String.fromInt dragItem ]

        Nothing ->
            Html.text ""


actualView : Model -> Html.Html Msg
actualView model =
    let
        cssVariables : String
        cssVariables =
            -- (+) 1 for the total
            [ ( "--columns-count", (List.length >> (+) 1 >> String.fromInt) model.cols )
            , ( "--rows-count", (List.length >> (+) 1 >> String.fromInt) model.rows )
            ]
                |> List.map (\( key, value ) -> key ++ ":" ++ value ++ ";")
                |> String.join ""
    in
    Html.main_
        [ moduleClass
            |> WeakCss.add "container"
            |> WeakCss.withStates [ ( "drag-drop-occurring", dnd.info model.dndModel.list /= Nothing ) ]
        ]
        [ Html.div
            [ moduleClass |> WeakCss.nestMany [ "container", "scrollable" ]
            , Html.Attributes.id scrollableContainerId
            , Html.Attributes.attribute "style" cssVariables
            , Html.Events.on "scroll" (Json.Decode.succeed OnScroll)
            , Html.Events.on "wheel" (Json.Decode.succeed OnScroll)
            ]
            [ Html.div
                [ moduleClass |> WeakCss.nestMany [ "container", "scrollable", "wrap" ] ]
                [ tableView model ]
            ]
        , ghostView model.dndModel
        ]


sampleView : Model -> Html.Html Msg
sampleView model =
    let
        cssVariables : String
        cssVariables =
            -- (+) 1 for the total
            [ ( "--columns-count", (List.length >> (+) 1 >> String.fromInt) model.cols )
            , ( "--rows-count", (List.length >> (+) 1 >> String.fromInt) model.rows )
            ]
                |> List.map (\( key, value ) -> key ++ ":" ++ value ++ ";")
                |> String.join ""
    in
    Html.main_
        [ moduleClass
            |> WeakCss.add "container"
            |> WeakCss.withStates [ ( "hidden", True ) ]
        ]
        [ Html.div
            [ moduleClass |> WeakCss.nestMany [ "container", "scrollable" ]
            , Html.Attributes.id scrollableContainerId
            , Html.Attributes.attribute "style" cssVariables
            ]
            [ Html.div
                [ moduleClass |> WeakCss.nestMany [ "container", "scrollable", "wrap" ] ]
                [ Html.div
                    [ moduleClass |> WeakCss.nest "table" ]
                    [ Html.div
                        [ moduleClass |> WeakCss.nestMany [ "table", "corner" ]
                        , Html.Attributes.id cornerCellId
                        ]
                        [ Html.text "corner" ]
                    , Html.div
                        [ moduleClass |> WeakCss.nestMany [ "table", "cells" ] ]
                        [ Html.ul
                            [ moduleClass |> WeakCss.nestMany [ "table", "cells", "row" ] ]
                            [ Html.li
                                [ moduleClass |> WeakCss.nestMany [ "table", "cells", "row", "item" ]
                                , Html.Attributes.id innerCellId
                                ]
                                [ Html.text "0 · 0" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ if model.sampleElements == Nothing then
            sampleView model

          else
            actualView model
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
