module Table exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Keyed
import Html.Lazy
import NewMove
import WeakCss


type alias Key =
    Int


total : Key
total =
    0


isTotal : Int -> Bool
isTotal index =
    index == 0



-- DND


type MovableList
    = Rows
    | Cols


scrollableContainerId : String
scrollableContainerId =
    "id-container-scroll"


dnd : NewMove.System Msg MovableList Key
dnd =
    NewMove.config
        |> NewMove.withContainer scrollableContainerId
        |> NewMove.create DnDMsg



-- MODEL


type alias Model =
    { dndModel : NewMove.Model MovableList Key
    , rows : List Key
    , cols : List Key
    }


init : ( Model, Cmd Msg )
init =
    ( { dndModel = dnd.model
      , rows = List.range 1 20
      , cols = List.range 21 50
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
    dnd.subscriptions model.dndModel



-- UPDATE


type Msg
    = DnDMsg (NewMove.Msg MovableList Key)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ dndModel } as model) =
    case msg of
        DnDMsg dndMsg ->
            let
                ( return, ( dndListModel, dndGhostModel ), dndCmd ) =
                    dnd.update dndMsg model.dndModel
            in
            case return of
                Just return_ ->
                    ( move return_ { model | dndModel = { dndModel | ghost = dndGhostModel, list = dndListModel } }, dndCmd )

                Nothing ->
                    if dndModel.list == dndListModel then
                        ( { model | dndModel = { dndModel | ghost = dndGhostModel } }, dndCmd )

                    else
                        ( { model | dndModel = { dndModel | ghost = dndGhostModel, list = dndListModel } }, dndCmd )



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


move : NewMove.Return MovableList Key -> Model -> Model
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
            |> WeakCss.addMany [ "table", headerClass, "item" ]
            |> WeakCss.withStates states
         , Html.Attributes.id htmlId
         ]
            ++ events
        )
        [ Html.text <| String.fromInt item ]


keyedHeaderView : MovableList -> HeaderClass -> NewMove.ListModel MovableList Key -> Int -> Key -> ( String, Html.Html Msg )
keyedHeaderView listId headerClass dndListModel index item =
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

        states : List ( String, Bool )
        states =
            case dnd.info dndListModel of
                Just { dragListId, dropListId, dragIndex, dropIndex, dragItem } ->
                    [ ( "total", isTotal index )
                    , ( "placeholder", dragIndex == index && dragListId == listId )
                    , ( "mouseover", dropIndex == index && dropListId == listId && dragItem /= item )
                    ]

                _ ->
                    [ ( "total", isTotal index ) ]

        events : List (Html.Attribute Msg)
        events =
            if dnd.info dndListModel == Nothing && not (isTotal index) then
                dnd.dragEvents listId item index htmlId

            else
                dnd.dropEvents listId index htmlId
    in
    ( htmlId, headerView headerClass states events item htmlId )


headersView : MovableList -> NewMove.ListModel MovableList Key -> List Key -> Html.Html Msg
headersView listId dndListModel list =
    let
        _ =
            Debug.log "headersView: check Html.Lazy" ""

        headerClass : HeaderClass
        headerClass =
            movableListToHeaderClass listId
    in
    (total :: list)
        |> List.indexedMap (keyedHeaderView listId headerClass dndListModel)
        |> Html.Keyed.node "ul" [ moduleClass |> WeakCss.nestMany [ "table", headerClass ] ]


cellView : Key -> Key -> Html.Html Msg
cellView row col =
    Html.li
        [ moduleClass
            |> WeakCss.addMany [ "table", "cells", "row", "item" ]
            |> WeakCss.withStates [ ( "total", row == total || col == total ) ]
        ]
        [ Html.text (String.fromInt row ++ " · " ++ String.fromInt col) ]


cellsView : List Key -> List Key -> Html.Html Msg
cellsView rows cols =
    let
        _ =
            Debug.log "cellsView: check Html.Lazy" ""
    in
    (total :: rows)
        |> List.map
            (\row ->
                (total :: cols)
                    |> List.map (\col -> cellView row col)
                    |> Html.ul [ moduleClass |> WeakCss.nestMany [ "table", "cells", "row" ] ]
            )
        |> Html.div [ moduleClass |> WeakCss.nestMany [ "table", "cells" ] ]


tableView : Model -> Html.Html Msg
tableView model =
    Html.div
        [ moduleClass |> WeakCss.nest "table" ]
        [ Html.div [ moduleClass |> WeakCss.nestMany [ "table", "corner" ] ] [ Html.text "corner" ]
        , Html.Lazy.lazy3 headersView Rows model.dndModel.list model.rows
        , Html.Lazy.lazy3 headersView Cols model.dndModel.list model.cols
        , Html.Lazy.lazy2 cellsView model.rows model.cols
        ]


ghostView : NewMove.Model MovableList Key -> Html.Html Msg
ghostView dndModel =
    case dnd.info dndModel.list of
        Just { dragItem } ->
            Html.div
                ((moduleClass |> WeakCss.nest "ghost") :: dnd.ghostStyles dndModel)
                [ Html.text <| String.fromInt dragItem ]

        Nothing ->
            Html.text ""


view : Model -> Html.Html Msg
view model =
    let
        cssVariables : String
        cssVariables =
            -- (+) 1 for the total
            [ ( "--columns-count", (List.length >> (+) 1 >> String.fromInt) model.cols ) ]
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
            ]
            [ Html.div
                [ moduleClass |> WeakCss.nestMany [ "container", "scrollable", "wrap" ] ]
                [ tableView model ]
            ]
        , ghostView model.dndModel
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
