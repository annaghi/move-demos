module Single exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Keyed
import List.Extra
import Move
import WeakCss



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


type alias Item =
    String


type alias Model =
    { dndModel : Move.Model () Item
    , list : List Item
    }


init : ( Model, Cmd Msg )
init =
    ( { dndModel = system.model
      , list = List.range 1 30 |> List.map (String.fromInt >> (++) "item-")
      }
    , Cmd.none
    )



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
                        | list = move dragIndex dropIndex model.list
                        , dndModel = dndModel
                      }
                    , dndCmd
                    )

                Nothing ->
                    ( { model
                        | dndModel = dndModel
                      }
                    , dndCmd
                    )



-- YOUR CUSTOM MOVE


move : Int -> Int -> List a -> List a
move dragIndex dropIndex list =
    if dragIndex < dropIndex then
        rotate dragIndex dropIndex list

    else if dragIndex > dropIndex then
        let
            n : Int
            n =
                List.length list - 1
        in
        List.reverse (rotate (n - dragIndex) (n - dropIndex) (List.reverse list))

    else
        list


rotate : Int -> Int -> List a -> List a
rotate i j list =
    let
        n : Int
        n =
            List.length list

        beginning : List a
        beginning =
            List.take i list

        middle : List a
        middle =
            list |> List.drop i |> List.take (j - i + 1)

        end : List a
        end =
            list |> List.reverse |> List.take (n - j - 1) |> List.reverse
    in
    beginning ++ rotateRecursive middle ++ end


rotateRecursive : List a -> List a
rotateRecursive list =
    case list of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: [ y ] ->
            y :: [ x ]

        x :: y :: rest ->
            y :: rotateRecursive (x :: rest)



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
            if system.info dndModel == Nothing then
                system.dragEvents () item index htmlId

            else
                system.dropEvents () index htmlId
    in
    ( htmlId, itemView dndModel states events item htmlId )


listView : Move.Model () Item -> List Item -> Html.Html Msg
listView dndModel list =
    list
        |> List.indexedMap (keyedItemView dndModel)
        |> Html.Keyed.node "ul" [ moduleClass |> WeakCss.nest "list" ]


ghostView : Move.Model () Item -> Html.Html Msg
ghostView dndModel =
    case system.info dndModel of
        Just { dragItem } ->
            Html.div
                ((moduleClass |> WeakCss.nest "ghost") :: system.ghostStyles dndModel)
                [ Html.text dragItem ]

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
            [ listView model.dndModel model.list ]
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
