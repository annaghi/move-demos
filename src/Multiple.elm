module Multiple exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Keyed
import List.Extra
import Move
import WeakCss



-- DATA


type alias Item =
    String


data1 : List Item
data1 =
    List.range 1 30 |> List.map (String.fromInt >> (++) "item-")


data2 : List Int
data2 =
    List.range 1 20



-- Move


type MovableListId
    = List1
    | List2


type MovableItem
    = Item1 Item
    | Item2 Int


scrollableContainerId : String
scrollableContainerId =
    "id-container-scroll"


system : Move.System Msg MovableListId MovableItem
system =
    Move.config
        |> Move.withContainer scrollableContainerId
        |> Move.create MoveMsg



-- CONVERSIONS


getClassFromId : MovableListId -> String
getClassFromId listId =
    case listId of
        List1 ->
            "list-1"

        List2 ->
            "list-2"



-- MODEL


type alias Model =
    { dndModel : Move.Model MovableListId MovableItem
    , list1 : List Item
    , list2 : List Int
    }


initialModel : Model
initialModel =
    { dndModel = system.model
    , list1 = data1
    , list2 = data2
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    system.subscriptions model.dndModel



-- UPDATE


type Msg
    = MoveMsg (Move.Msg MovableListId MovableItem)


insertAt : Int -> item -> List item -> List item
insertAt i item list =
    List.take i list ++ (item :: List.drop i list)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveMsg moveMsg ->
            let
                ( return, dndModel, dndCmd ) =
                    system.update moveMsg model.dndModel
            in
            case return of
                Just { dragListId, dropListId, dragIndex, dropIndex, dragItem } ->
                    case ( dropListId, dragItem ) of
                        ( List1, Item1 item1 ) ->
                            ( { model
                                | list1 =
                                    if dragIndex < dropIndex then
                                        model.list1 |> List.filter ((/=) item1) |> insertAt (dropIndex - 1) item1

                                    else
                                        model.list1 |> List.filter ((/=) item1) |> insertAt dropIndex item1
                                , dndModel = dndModel
                              }
                            , dndCmd
                            )

                        ( List2, Item2 item2 ) ->
                            ( { model
                                | list2 =
                                    if dragIndex < dropIndex then
                                        model.list2 |> List.filter ((/=) item2) |> insertAt (dropIndex - 1) item2

                                    else
                                        model.list2 |> List.filter ((/=) item2) |> insertAt dropIndex item2
                                , dndModel = dndModel
                              }
                            , dndCmd
                            )

                        ( List1, Item2 item2 ) ->
                            ( { model
                                | list1 = insertAt dropIndex ((String.fromInt >> (++) "ITEM-") item2) model.list1
                                , list2 = List.filter ((/=) item2) model.list2
                                , dndModel = dndModel
                              }
                            , dndCmd
                            )

                        ( List2, Item1 item1 ) ->
                            ( { model
                                | list1 = List.filter ((/=) item1) model.list1
                                , list2 = insertAt dropIndex ((String.dropLeft 5 >> String.toInt >> Maybe.withDefault -1 >> (*) 100) item1) model.list2
                                , dndModel = dndModel
                              }
                            , dndCmd
                            )

                Nothing ->
                    ( { model | dndModel = dndModel }
                    , dndCmd
                    )



-- VIEW


moduleClass : WeakCss.ClassName
moduleClass =
    WeakCss.namespace "m"


item1View : Move.Model MovableListId MovableItem -> List ( String, Bool ) -> List (Html.Attribute Msg) -> MovableListId -> Item -> String -> Html.Html Msg
item1View dndModel states events listId item htmlId =
    Html.li
        ([ moduleClass |> WeakCss.addMany [ "list", "item" ] |> WeakCss.withStates states
         , Html.Attributes.id htmlId
         ]
            ++ events
        )
        [ Html.text item ]


keyedItem1View : Move.Model MovableListId MovableItem -> MovableListId -> Int -> Item -> ( String, Html.Html Msg )
keyedItem1View dndModel listId index item =
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
                    [ ( "placeholder", dragIndex == index && dragListId == listId )
                    , ( "mouseover", dropIndex == index && dropListId == listId && dragItem /= Item1 item )
                    ]

                _ ->
                    []

        events : List (Html.Attribute Msg)
        events =
            if system.info dndModel == Nothing && not isFirstItem then
                system.dragEvents listId (Item1 item) index htmlId

            else
                system.dropEvents listId index htmlId
    in
    ( htmlId, item1View dndModel states events listId item htmlId )


item2View : Move.Model MovableListId MovableItem -> List ( String, Bool ) -> List (Html.Attribute Msg) -> MovableListId -> Int -> String -> Html.Html Msg
item2View dndModel states events listId item htmlId =
    Html.li
        ([ moduleClass |> WeakCss.addMany [ "list", "item" ] |> WeakCss.withStates states
         , Html.Attributes.id htmlId
         ]
            ++ events
        )
        [ Html.text <| String.fromInt item ]


keyedItem2View : Move.Model MovableListId MovableItem -> MovableListId -> Int -> Int -> ( String, Html.Html Msg )
keyedItem2View dndModel listId index item =
    let
        htmlId : String
        htmlId =
            String.fromInt item

        isFirstItem : Bool
        isFirstItem =
            index == 0

        states : List ( String, Bool )
        states =
            case system.info dndModel of
                Just { dragListId, dropListId, dragIndex, dropIndex, dragItem } ->
                    [ ( "placeholder", dragIndex == index && dragListId == listId )
                    , ( "mouseover", dropIndex == index && dropListId == listId && dragItem /= Item2 item )
                    ]

                _ ->
                    []

        events : List (Html.Attribute Msg)
        events =
            if system.info dndModel == Nothing && not isFirstItem then
                system.dragEvents listId (Item2 item) index htmlId

            else
                system.dropEvents listId index htmlId
    in
    ( htmlId, item2View dndModel states events listId item htmlId )


ghostView : Move.Model MovableListId MovableItem -> Html.Html Msg
ghostView dndModel =
    case system.info dndModel of
        Just { dragItem } ->
            case dragItem of
                Item1 item1 ->
                    Html.div
                        ((moduleClass |> WeakCss.nestMany [ "list", "ghost" ]) :: system.ghostStyles dndModel)
                        [ Html.text item1 ]

                Item2 item2 ->
                    Html.div
                        ((moduleClass |> WeakCss.nestMany [ "list", "ghost" ]) :: system.ghostStyles dndModel)
                        [ Html.text <| String.fromInt item2 ]

        Nothing ->
            Html.text ""


view : Model -> Html.Html Msg
view model =
    Html.main_
        [ moduleClass |> WeakCss.add "main" |> WeakCss.withStates [ ( "drag-drop-occurring", system.info model.dndModel /= Nothing ) ] ]
        [ Html.div
            [ moduleClass |> WeakCss.nest "container-scroll"
            , Html.Attributes.id scrollableContainerId
            ]
            ([ ("list-1-static-item" :: model.list1)
                |> List.indexedMap (keyedItem1View model.dndModel List1)
                |> Html.Keyed.node "ul" [ moduleClass |> WeakCss.nestMany [ "list" ] ]
             , (0 :: model.list2)
                |> List.indexedMap (keyedItem2View model.dndModel List2)
                |> Html.Keyed.node "ul" [ moduleClass |> WeakCss.nestMany [ "list" ] ]
             ]
                ++ [ ghostView model.dndModel ]
            )
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
