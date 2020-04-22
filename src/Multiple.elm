module Multiple exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Keyed
import Move
import WeakCss



-- DND


type MovableList
    = List1
    | List2


type MovableItem
    = Item1 String
    | Item2 Int


scrollableContainerId : String
scrollableContainerId =
    "id-container-scroll"


dnd : Move.System Msg MovableList MovableItem
dnd =
    Move.config
        |> Move.withContainer scrollableContainerId
        |> Move.create DnDMsg



-- MODEL


type alias Model =
    { dndModel : Move.Model MovableList MovableItem
    , list1 : List String
    , list2 : List Int
    }


init : ( Model, Cmd Msg )
init =
    ( { dndModel = dnd.model
      , list1 = List.range 1 30 |> List.map (String.fromInt >> (++) "old-")
      , list2 = List.range 1 20
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    dnd.subscriptions model.dndModel



-- UPDATE


type Msg
    = DnDMsg (Move.Msg MovableList MovableItem)


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


item1ToItem2 : String -> Int
item1ToItem2 =
    String.dropLeft 4 >> String.toInt >> Maybe.withDefault -1 >> (*) 10


item2ToItem1 : Int -> String
item2ToItem1 =
    String.fromInt >> (++) "new-"


insertAt : Int -> item -> List item -> List item
insertAt index item list =
    List.take index list ++ (item :: List.drop index list)


move : Move.Return MovableList MovableItem -> Model -> Model
move { dragIndex, dragItem, dropList, dropIndex } model =
    case ( dragItem, dropList ) of
        ( Item1 item1, List1 ) ->
            { model
                | list1 =
                    if dragIndex < dropIndex then
                        model.list1 |> List.filter ((/=) item1) |> insertAt (dropIndex - 1) item1

                    else
                        model.list1 |> List.filter ((/=) item1) |> insertAt dropIndex item1
            }

        ( Item2 item2, List2 ) ->
            { model
                | list2 =
                    if dragIndex < dropIndex then
                        model.list2 |> List.filter ((/=) item2) |> insertAt (dropIndex - 1) item2

                    else
                        model.list2 |> List.filter ((/=) item2) |> insertAt dropIndex item2
            }

        ( Item1 item1, List2 ) ->
            { model
                | list1 = List.filter ((/=) item1) model.list1
                , list2 = insertAt dropIndex (item1ToItem2 item1) model.list2
            }

        ( Item2 item2, List1 ) ->
            { model
                | list1 = insertAt dropIndex (item2ToItem1 item2) model.list1
                , list2 = List.filter ((/=) item2) model.list2
            }



-- VIEW


moduleClass : WeakCss.ClassName
moduleClass =
    WeakCss.namespace "m"



-- List 1


item1View : List ( String, Bool ) -> List (Html.Attribute Msg) -> String -> String -> Html.Html Msg
item1View states events item htmlId =
    Html.li
        ([ moduleClass |> WeakCss.addMany [ "list", "item" ] |> WeakCss.withStates states
         , Html.Attributes.id htmlId
         ]
            ++ events
        )
        [ Html.text item ]


keyedItem1View : Move.Model MovableList MovableItem -> Int -> String -> ( String, Html.Html Msg )
keyedItem1View dndModel index item =
    let
        htmlId : String
        htmlId =
            item

        isFirstItem : Bool
        isFirstItem =
            index == 0

        states : List ( String, Bool )
        states =
            case dnd.info dndModel of
                Just { dragList, dropList, dragIndex, dropIndex, dragItem } ->
                    [ ( "placeholder", dragIndex == index && dragList == List1 )
                    , ( "mouseover", dropIndex == index && dropList == List1 && dragItem /= Item1 item )
                    ]

                _ ->
                    []

        events : List (Html.Attribute Msg)
        events =
            if dnd.info dndModel == Nothing && not isFirstItem then
                dnd.dragEvents List1 (Item1 item) index htmlId

            else
                dnd.dropEvents List1 index htmlId
    in
    ( htmlId, item1View states events item htmlId )


list1View : Move.Model MovableList MovableItem -> List String -> Html.Html Msg
list1View dndModel list =
    list
        |> List.indexedMap (keyedItem1View dndModel)
        |> Html.Keyed.node "ul" [ moduleClass |> WeakCss.nest "list" ]



-- List 2


item2View : List ( String, Bool ) -> List (Html.Attribute Msg) -> Int -> String -> Html.Html Msg
item2View states events item htmlId =
    Html.li
        ([ moduleClass |> WeakCss.addMany [ "list", "item" ] |> WeakCss.withStates states
         , Html.Attributes.id htmlId
         ]
            ++ events
        )
        [ Html.text <| String.fromInt item ]


keyedItem2View : Move.Model MovableList MovableItem -> Int -> Int -> ( String, Html.Html Msg )
keyedItem2View dndModel index item =
    let
        htmlId : String
        htmlId =
            "number-" ++ String.fromInt item

        isFirstItem : Bool
        isFirstItem =
            index == 0

        states : List ( String, Bool )
        states =
            case dnd.info dndModel of
                Just { dragList, dropList, dragIndex, dropIndex, dragItem } ->
                    [ ( "placeholder", dragIndex == index && dragList == List2 )
                    , ( "mouseover", dropIndex == index && dropList == List2 && dragItem /= Item2 item )
                    ]

                _ ->
                    []

        events : List (Html.Attribute Msg)
        events =
            if dnd.info dndModel == Nothing && not isFirstItem then
                dnd.dragEvents List2 (Item2 item) index htmlId

            else
                dnd.dropEvents List2 index htmlId
    in
    ( htmlId, item2View states events item htmlId )


list2View : Move.Model MovableList MovableItem -> List Int -> Html.Html Msg
list2View dndModel list =
    list
        |> List.indexedMap (keyedItem2View dndModel)
        |> Html.Keyed.node "ul" [ moduleClass |> WeakCss.nest "list" ]



-- Ghost


ghostView : Move.Model MovableList MovableItem -> Html.Html Msg
ghostView dndModel =
    case dnd.info dndModel of
        Just { dragItem } ->
            case dragItem of
                Item1 item1 ->
                    Html.div
                        ((moduleClass |> WeakCss.nest "ghost") :: dnd.ghostStyles dndModel)
                        [ Html.text item1 ]

                Item2 item2 ->
                    Html.div
                        ((moduleClass |> WeakCss.nest "ghost") :: dnd.ghostStyles dndModel)
                        [ Html.text <| String.fromInt item2 ]

        Nothing ->
            Html.text ""


view : Model -> Html.Html Msg
view model =
    Html.main_
        [ moduleClass
            |> WeakCss.add "container"
            |> WeakCss.withStates [ ( "drag-drop-occurring", dnd.info model.dndModel /= Nothing ) ]
        ]
        [ Html.div
            [ moduleClass |> WeakCss.nestMany [ "container", "scrollable" ]
            , Html.Attributes.id scrollableContainerId
            ]
            [ Html.div
                [ moduleClass |> WeakCss.nestMany [ "container", "scrollable", "wrap" ] ]
                [ list1View model.dndModel ("list-1-static-item" :: model.list1)
                , list2View model.dndModel (0 :: model.list2)
                ]
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
