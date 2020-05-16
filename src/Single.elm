module Single exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Keyed
import Html.Lazy
import Move
import WeakCss



-- DND


type alias Item =
    String


scrollableContainerId : String
scrollableContainerId =
    "id-container-scroll"


dnd : Move.System Msg () Item
dnd =
    Move.config
        |> Move.withContainer scrollableContainerId
        |> Move.create DnDMsg



-- MODEL


type alias Model =
    { dndModel : Move.Model () Item
    , list : List Item
    }


init : ( Model, Cmd Msg )
init =
    ( { dndModel = dnd.model
      , list = List.range 1 30 |> List.map (String.fromInt >> (++) "item-")
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    dnd.subscriptions model.dndModel



-- UPDATE


type Msg
    = DnDMsg (Move.Msg () Item)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DnDMsg dndMsg ->
            let
                ( return, ( dndListModel, dndGhostModel ), dndCmd ) =
                    dnd.update dndMsg model.dndModel

                updateLazyDnDModel : Move.Model () Item -> Move.Model () Item
                updateLazyDnDModel dndModel =
                    if dndModel.list == dndListModel then
                        { dndModel | ghost = dndGhostModel }

                    else
                        { dndModel | list = dndListModel, ghost = dndGhostModel }
            in
            case return of
                Just { dragIndex, dropIndex } ->
                    ( { model
                        | list = reorder dragIndex dropIndex model.list
                        , dndModel = Move.Model dndListModel dndGhostModel
                      }
                    , dndCmd
                    )

                Nothing ->
                    ( { model
                        | dndModel = updateLazyDnDModel model.dndModel
                      }
                    , dndCmd
                    )



-- YOUR CUSTOM REORDER


reorder : Int -> Int -> List item -> List item
reorder dragIndex dropIndex list =
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


rotate : Int -> Int -> List item -> List item
rotate i j list =
    let
        n : Int
        n =
            List.length list

        beginning : List item
        beginning =
            List.take i list

        middle : List item
        middle =
            list |> List.drop i |> List.take (j - i + 1)

        end : List item
        end =
            list |> List.reverse |> List.take (n - j - 1) |> List.reverse
    in
    beginning ++ rotateRecursive middle ++ end


rotateRecursive : List item -> List item
rotateRecursive list =
    case list of
        [] ->
            []

        [ x ] ->
            [ x ]

        [ x, y ] ->
            [ y, x ]

        x :: y :: rest ->
            y :: rotateRecursive (x :: rest)



-- VIEW


moduleClass : WeakCss.ClassName
moduleClass =
    WeakCss.namespace "s"


itemView : List ( String, Bool ) -> List (Html.Attribute Msg) -> Item -> String -> Html.Html Msg
itemView states events item htmlId =
    Html.li
        ([ moduleClass |> WeakCss.addMany [ "list", "item" ] |> WeakCss.withStates states
         , Html.Attributes.id htmlId
         ]
            ++ events
        )
        [ Html.text item ]


keyedItemView : Move.ListModel () Item -> Int -> Item -> ( String, Html.Html Msg )
keyedItemView dndListModel index item =
    let
        htmlId : String
        htmlId =
            item

        states : List ( String, Bool )
        states =
            case dnd.info dndListModel of
                Just { dragIndex, dropIndex } ->
                    [ ( "placeholder", dragIndex == index )
                    , ( "mouseover", dropIndex == index && dragIndex /= index )
                    ]

                _ ->
                    []

        events : List (Html.Attribute Msg)
        events =
            if dnd.info dndListModel == Nothing then
                dnd.dragEvents () item index htmlId

            else
                dnd.dropEvents () index htmlId
    in
    ( htmlId, itemView states events item htmlId )


listView : Move.ListModel () Item -> List Item -> Html.Html Msg
listView dndListModel list =
    let
        _ =
            Debug.log "listView checking Html.Lazy" ""
    in
    list
        |> List.indexedMap (keyedItemView dndListModel)
        |> Html.Keyed.node "ul" [ moduleClass |> WeakCss.nest "list" ]


ghostView : Move.Model () Item -> Html.Html Msg
ghostView dndModel =
    case dnd.info dndModel.list of
        Just { dragItem } ->
            Html.div
                ((moduleClass |> WeakCss.nest "ghost") :: dnd.ghostStyles dndModel)
                [ Html.text dragItem ]

        Nothing ->
            Html.text ""


view : Model -> Html.Html Msg
view model =
    Html.main_
        [ moduleClass
            |> WeakCss.add "container"
            |> WeakCss.withStates [ ( "drag-drop-occurring", dnd.info model.dndModel.list /= Nothing ) ]
        ]
        [ Html.div
            [ moduleClass |> WeakCss.nestMany [ "container", "scrollable" ]
            , Html.Attributes.id scrollableContainerId
            ]
            -- Just remove Html.Lazy.lazy2 here and check the difference in Debug.logs
            [ Html.Lazy.lazy2 listView model.dndModel.list model.list ]
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
