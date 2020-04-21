module Multiple exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Keyed
import List.Extra
import Move
import WeakCss



-- Move


type MovableListId
    = List1
    | List2


type MovableItem
    = Item1 String
    | Item2 Int


scrollableContainerId : String
scrollableContainerId =
    "id-container-scroll"


system : Move.System Msg MovableListId MovableItem
system =
    Move.config
        |> Move.withContainer scrollableContainerId
        |> Move.create MoveMsg



-- MODEL


type alias Model =
    { dndModel : Move.Model MovableListId MovableItem
    , list1 : List String
    , list2 : List Int
    }


init : ( Model, Cmd Msg )
init =
    ( { dndModel = system.model
      , list1 = List.range 1 30 |> List.map (String.fromInt >> (++) "old-")
      , list2 = List.range 1 20
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    system.subscriptions model.dndModel



-- UPDATE


type Msg
    = MoveMsg (Move.Msg MovableListId MovableItem)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveMsg moveMsg ->
            let
                ( return, dndModel, dndCmd ) =
                    system.update moveMsg model.dndModel
            in
            case return of
                Just return_ ->
                    ( move return_ { model | dndModel = dndModel }, dndCmd )

                Nothing ->
                    ( { model | dndModel = dndModel }, dndCmd )



-- YOUR CUSTOM MOVE


convertItem1ToItem2 : String -> Int
convertItem1ToItem2 =
    String.dropLeft 4 >> String.toInt >> Maybe.withDefault -1 >> (*) 10


convertItem2ToItem1 : Int -> String
convertItem2ToItem1 =
    String.fromInt >> (++) "new-"


insertAt : Int -> item -> List item -> List item
insertAt index item list =
    List.take index list ++ (item :: List.drop index list)


move : Move.Return MovableListId MovableItem -> Model -> Model
move { dragIndex, dragItem, dropListId, dropIndex } model =
    case ( dragItem, dropListId ) of
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

        ( Item2 item2, List1 ) ->
            { model
                | list1 = insertAt dropIndex (convertItem2ToItem1 item2) model.list1
                , list2 = List.filter ((/=) item2) model.list2
            }

        ( Item1 item1, List2 ) ->
            { model
                | list1 = List.filter ((/=) item1) model.list1
                , list2 = insertAt dropIndex (convertItem1ToItem2 item1) model.list2
            }



-- VIEW


moduleClass : WeakCss.ClassName
moduleClass =
    WeakCss.namespace "m"


item1View : List ( String, Bool ) -> List (Html.Attribute Msg) -> String -> String -> Html.Html Msg
item1View states events item htmlId =
    Html.li
        ([ moduleClass |> WeakCss.addMany [ "list", "item" ] |> WeakCss.withStates states
         , Html.Attributes.id htmlId
         ]
            ++ events
        )
        [ Html.text item ]


keyedItem1View : Move.Model MovableListId MovableItem -> Int -> String -> ( String, Html.Html Msg )
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
            case system.info dndModel of
                Just { dragListId, dropListId, dragIndex, dropIndex, dragItem } ->
                    [ ( "placeholder", dragIndex == index && dragListId == List1 )
                    , ( "mouseover", dropIndex == index && dropListId == List1 && dragItem /= Item1 item )
                    ]

                _ ->
                    []

        events : List (Html.Attribute Msg)
        events =
            if system.info dndModel == Nothing && not isFirstItem then
                system.dragEvents List1 (Item1 item) index htmlId

            else
                system.dropEvents List1 index htmlId
    in
    ( htmlId, item1View states events item htmlId )


list1View : Move.Model MovableListId MovableItem -> List String -> Html.Html Msg
list1View dndModel list =
    list
        |> List.indexedMap (keyedItem1View dndModel)
        |> Html.Keyed.node "ul" [ moduleClass |> WeakCss.nest "list" ]


item2View : List ( String, Bool ) -> List (Html.Attribute Msg) -> Int -> String -> Html.Html Msg
item2View states events item htmlId =
    Html.li
        ([ moduleClass |> WeakCss.addMany [ "list", "item" ] |> WeakCss.withStates states
         , Html.Attributes.id htmlId
         ]
            ++ events
        )
        [ Html.text <| String.fromInt item ]


keyedItem2View : Move.Model MovableListId MovableItem -> Int -> Int -> ( String, Html.Html Msg )
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
            case system.info dndModel of
                Just { dragListId, dropListId, dragIndex, dropIndex, dragItem } ->
                    [ ( "placeholder", dragIndex == index && dragListId == List2 )
                    , ( "mouseover", dropIndex == index && dropListId == List2 && dragItem /= Item2 item )
                    ]

                _ ->
                    []

        events : List (Html.Attribute Msg)
        events =
            if system.info dndModel == Nothing && not isFirstItem then
                system.dragEvents List2 (Item2 item) index htmlId

            else
                system.dropEvents List2 index htmlId
    in
    ( htmlId, item2View states events item htmlId )


list2View : Move.Model MovableListId MovableItem -> List Int -> Html.Html Msg
list2View dndModel list =
    list
        |> List.indexedMap (keyedItem2View dndModel)
        |> Html.Keyed.node "ul" [ moduleClass |> WeakCss.nest "list" ]


ghostView : Move.Model MovableListId MovableItem -> Html.Html Msg
ghostView dndModel =
    case system.info dndModel of
        Just { dragItem } ->
            case dragItem of
                Item1 item1 ->
                    Html.div
                        ((moduleClass |> WeakCss.nest "ghost") :: system.ghostStyles dndModel)
                        [ Html.text item1 ]

                Item2 item2 ->
                    Html.div
                        ((moduleClass |> WeakCss.nest "ghost") :: system.ghostStyles dndModel)
                        [ Html.text <| String.fromInt item2 ]

        Nothing ->
            Html.text ""


view : Model -> Html.Html Msg
view model =
    Html.main_
        [ moduleClass
            |> WeakCss.add "main"
            |> WeakCss.withStates [ ( "drag-drop-occurring", system.info model.dndModel /= Nothing ) ]
        ]
        [ Html.div
            [ moduleClass |> WeakCss.nest "container-scroll"
            , Html.Attributes.id scrollableContainerId
            ]
            [ Html.div
                [ moduleClass |> WeakCss.nest "container" ]
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
