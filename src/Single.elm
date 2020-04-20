module Single exposing (main)

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


data : List Item
data =
    List.range 1 30 |> List.map (String.fromInt >> (++) "item-")



-- Move


scrollableContainerId : String
scrollableContainerId =
    "id-container-scroll"


system : Move.System Msg () Item
system =
    Move.config
        |> Move.withContainer scrollableContainerId
        |> Move.create MoveMsg



-- MODEL


type alias Model =
    { dndModel : Move.Model () Item
    , list : List Item
    }


initialModel : Model
initialModel =
    { dndModel = system.model
    , list = data
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
    = MoveMsg (Move.Msg () Item)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveMsg moveMsg ->
            let
                ( return, dndModel, dndCmd ) =
                    system.update moveMsg model.dndModel
            in
            case return of
                Just { dragIndex, dropIndex, dragItem } ->
                    ( { model
                        | list =
                            if dragIndex < dropIndex then
                                model.list |> List.filter ((/=) dragItem) |> insertAt (dropIndex - 1) dragItem

                            else
                                model.list |> List.filter ((/=) dragItem) |> insertAt dropIndex dragItem
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
    WeakCss.namespace "s"


itemView : Move.Model () Item -> List ( String, Bool ) -> List (Html.Attribute Msg) -> Item -> String -> Html.Html Msg
itemView dndModel states events item htmlId =
    Html.li
        ([ moduleClass |> WeakCss.addMany [ "list", "item" ] |> WeakCss.withStates states
         , Html.Attributes.id htmlId
         ]
            ++ events
        )
        [ Html.text item ]


keyedItemView : Move.Model () Item -> Int -> Item -> ( String, Html.Html Msg )
keyedItemView dndModel index item =
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
                Just { dragIndex, dropIndex } ->
                    [ ( "placeholder", dragIndex == index )
                    , ( "mouseover", dropIndex == index && dragIndex /= index )
                    ]

                _ ->
                    []

        events : List (Html.Attribute Msg)
        events =
            if system.info dndModel == Nothing && not isFirstItem then
                system.dragEvents () item index htmlId

            else
                system.dropEvents () index htmlId
    in
    ( htmlId, itemView dndModel states events item htmlId )


ghostView : Move.Model () Item -> Html.Html Msg
ghostView dndModel =
    case system.info dndModel of
        Just { dragItem } ->
            Html.div
                ((moduleClass |> WeakCss.nestMany [ "list", "ghost" ]) :: system.ghostStyles dndModel)
                [ Html.text dragItem ]

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
            ([ ("list-static-item" :: model.list)
                |> List.indexedMap (keyedItemView model.dndModel)
                |> Html.Keyed.node "ul" [ moduleClass |> WeakCss.nest "list" ]
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



-- OPERATION


insertAt : Int -> item -> List item -> List item
insertAt i item list =
    List.take i list ++ (item :: List.drop i list)
