module Table exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Keyed
import Html.Lazy
import Move
import WeakCss



-- DND


type alias Key =
    Int


type MovableList
    = Rows
    | Cols


scrollableContainerId : String
scrollableContainerId =
    "id-container-scroll"


dnd : Move.System Msg MovableList Key
dnd =
    Move.config
        |> Move.withContainer scrollableContainerId
        |> Move.create DnDMsg



-- MODEL


type alias Model =
    { dndModel : Move.Model MovableList Key
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



-- CONVERSATION


type alias HeaderClass =
    String


movableListToHeaderClass : MovableList -> HeaderClass
movableListToHeaderClass listId =
    case listId of
        Rows ->
            "rows"

        Cols ->
            "cols"


headerCaption : Int -> String
headerCaption =
    String.fromInt



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    dnd.subscriptions model.dndModel



-- UPDATE


type Msg
    = DnDMsg (Move.Msg MovableList Key)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DnDMsg dndMsg ->
            let
                ( return, dndModel, dndCmd ) =
                    dnd.update dndMsg model.dndModel
            in
            case return of
                Just return_ ->
                    ( move return_ { model | dndModel = dndModel }, dndCmd )

                Nothing ->
                    ( { model | dndModel = dndModel }, dndCmd )



-- YOUR CUSTOM MOVE


insertAt : Int -> item -> List item -> List item
insertAt index item list =
    List.take index list ++ (item :: List.drop index list)


move : Move.Return MovableList Key -> Model -> Model
move { dragList, dragIndex, dragItem, dropList, dropIndex } model =
    case ( dragList, dropList ) of
        ( Rows, Rows ) ->
            { model
                | rows =
                    if dragIndex < dropIndex then
                        model.rows |> List.filter ((/=) dragItem) |> insertAt (dropIndex - 1) dragItem

                    else
                        model.rows |> List.filter ((/=) dragItem) |> insertAt dropIndex dragItem
            }

        ( Cols, Cols ) ->
            { model
                | cols =
                    if dragIndex < dropIndex then
                        model.cols |> List.filter ((/=) dragItem) |> insertAt (dropIndex - 1) dragItem

                    else
                        model.cols |> List.filter ((/=) dragItem) |> insertAt dropIndex dragItem
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
        [ Html.text <| headerCaption item ]


keyedHeaderView : MovableList -> HeaderClass -> Move.Model MovableList Key -> Int -> Key -> ( String, Html.Html Msg )
keyedHeaderView listId headerClass dndModel index item =
    let
        isTotal : Bool
        isTotal =
            index == 0

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
            case dnd.info dndModel of
                Just { dragList, dropList, dragIndex, dropIndex, dragItem } ->
                    [ ( "total", isTotal )
                    , ( "placeholder", dragIndex == index && dragList == listId )
                    , ( "mouseover", dropIndex == index && dropList == listId && dragItem /= item )
                    ]

                _ ->
                    [ ( "total", isTotal ) ]

        events : List (Html.Attribute Msg)
        events =
            if dnd.info dndModel == Nothing && not isTotal then
                dnd.dragEvents listId item index htmlId

            else
                dnd.dropEvents listId index htmlId
    in
    ( htmlId, headerView headerClass states events item htmlId )


headersView : MovableList -> Move.Model MovableList Key -> List Key -> Html.Html Msg
headersView listId dndModel list =
    let
        headerClass : HeaderClass
        headerClass =
            movableListToHeaderClass listId
    in
    list
        |> List.indexedMap (keyedHeaderView listId headerClass dndModel)
        |> Html.Keyed.node "ul" [ moduleClass |> WeakCss.nestMany [ "table", headerClass ] ]


cellView : Key -> Key -> Html.Html Msg
cellView row col =
    Html.li
        [ moduleClass
            |> WeakCss.addMany [ "table", "cells", "row", "item" ]
            |> WeakCss.withStates [ ( "total", row == 0 || col == 0 ) ]
        ]
        [ Html.text (String.fromInt row ++ " / " ++ String.fromInt col) ]


cellsView : Key -> List Key -> List Key -> Html.Html Msg
cellsView total rows cols =
    let
        _ =
            Debug.log "check Html.Lazy" ""
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
    let
        total : Key
        total =
            0
    in
    Html.div
        [ moduleClass |> WeakCss.nest "table" ]
        [ Html.div [ moduleClass |> WeakCss.nestMany [ "table", "corner" ] ] [ Html.text "corner" ]
        , headersView Rows model.dndModel (total :: model.rows)
        , headersView Cols model.dndModel (total :: model.cols)
        , Html.Lazy.lazy3 cellsView total model.rows model.cols
        ]


ghostView : Move.Model MovableList Key -> Html.Html Msg
ghostView dndModel =
    case dnd.info dndModel of
        Just { dragItem } ->
            Html.div
                ((moduleClass |> WeakCss.nest "ghost") :: dnd.ghostStyles dndModel)
                [ Html.text <| headerCaption dragItem ]

        Nothing ->
            Html.text ""


view : Model -> Html.Html Msg
view model =
    let
        cssVariables : String
        cssVariables =
            -- (+) 1 for the total
            [ ( "--columns-count", (String.fromInt << (+) 1 << List.length) model.cols ) ]
                |> List.map (\( key, value ) -> key ++ ":" ++ value ++ ";")
                |> String.join ""
    in
    Html.main_
        [ moduleClass
            |> WeakCss.add "main"
            |> WeakCss.withStates [ ( "drag-drop-occurring", dnd.info model.dndModel /= Nothing ) ]
        ]
        [ Html.div
            [ moduleClass |> WeakCss.nest "container-scroll"
            , Html.Attributes.id scrollableContainerId
            , Html.Attributes.attribute "style" cssVariables
            ]
            [ Html.div
                [ moduleClass |> WeakCss.nest "container" ]
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
