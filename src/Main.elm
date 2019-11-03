module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import FeatherIcons
import Html exposing (Html, button, div, hr, input, li, text, ul)
import Html.Attributes exposing (autofocus, class, classList, disabled, value)
import Html.Events exposing (onBlur, onClick, onInput)
import List.Extra
import Random
import Random.List exposing (shuffle)
import Task
import Time


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


replace : a -> a -> List a -> List a
replace oldA newA =
    List.map
        (\ax ->
            if ax == oldA then
                newA

            else
                ax
        )



-- MODEL


type alias Id =
    Int


type alias Nonce =
    Int


type CardType
    = Zero
    | One
    | MinusOne
    | Two
    | MinusTwo
    | Crit
    | Null


type alias Card =
    { id : Id
    , cardType : CardType
    }


type alias Pile =
    { id : Id
    , cards : List Card
    }


type EditState
    = Default
    | Editing


type alias Mat =
    { id : Id
    , deck : Pile
    , discard : Pile
    , cardEditState : EditState
    , nameEditState : EditState
    , name : String
    }


type alias Model =
    { mats : List Mat
    , nonce : Nonce
    , seed : Random.Seed
    }


stringForCardType : CardType -> String
stringForCardType cardType =
    case cardType of
        Zero ->
            "Zero"

        One ->
            "One"

        MinusOne ->
            "MinusOne"

        Two ->
            "Two"

        MinusTwo ->
            "MinusTwo"

        Crit ->
            "Crit"

        Null ->
            "Null"


makeDefaultCards : Nonce -> ( Nonce, List Card )
makeDefaultCards nonce =
    ( nonce + 7
    , [ Card (nonce + 0) Zero
      , Card (nonce + 1) One
      , Card (nonce + 2) MinusOne
      , Card (nonce + 3) Two
      , Card (nonce + 4) MinusTwo
      , Card (nonce + 5) Crit
      , Card (nonce + 6) Null
      ]
    )


defaultMat : Mat
defaultMat =
    { id = 1
    , deck = { id = 2, cards = [] }
    , discard = { id = 3, cards = [] }
    , cardEditState = Editing
    , nameEditState = Default
    , name = "Mat 1"
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mats = [ defaultMat ]
      , nonce = 4
      , seed = Random.initialSeed 0
      }
    , Task.perform GenerateSeed Time.now
    )



-- UPDATE


type Msg
    = GenerateSeed Time.Posix
    | AddMat
      -- mat operations
    | Reshuffle Mat
    | Draw Mat
    | AddCard Mat CardType
    | RemoveCard Pile Mat Card
    | ToggleMatCardEdit Mat
    | ToggleMatNameEdit Mat
    | AddDefaultCards Mat
    | RemoveAllCards Mat
    | HandleMatNameInput Mat String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateSeed time ->
            ( { model | seed = Random.initialSeed (Time.posixToMillis time) }, Cmd.none )

        AddMat ->
            let
                newMat : Mat
                newMat =
                    { id = model.nonce
                    , deck = { id = model.nonce + 1, cards = [] }
                    , discard = { id = model.nonce + 2, cards = [] }
                    , cardEditState = Default
                    , nameEditState = Default
                    , name = "Mat " ++ String.fromInt (List.length model.mats + 1)
                    }
            in
            ( { model | mats = model.mats ++ [ newMat ], nonce = model.nonce + 3 }, Cmd.none )

        Reshuffle mat ->
            let
                allCards : List Card
                allCards =
                    List.concat [ mat.deck.cards, mat.discard.cards ]

                ( shuffledCards, newSeed ) =
                    Random.step (shuffle allCards) model.seed

                oldDeck : Pile
                oldDeck =
                    mat.deck

                newDeck : Pile
                newDeck =
                    { oldDeck | cards = shuffledCards }

                oldDiscard : Pile
                oldDiscard =
                    mat.discard

                newDiscard : Pile
                newDiscard =
                    { oldDiscard | cards = [] }

                newMat : Mat
                newMat =
                    { mat | deck = newDeck, discard = newDiscard }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats
            in
            ( { model | mats = newMats, seed = newSeed }, Cmd.none )

        Draw mat ->
            case mat.deck.cards of
                [] ->
                    ( model, Cmd.none )

                firstCard :: restOfCards ->
                    let
                        ( drawnCard, newSeed ) =
                            Random.step (Random.uniform firstCard restOfCards) model.seed

                        oldDiscard : Pile
                        oldDiscard =
                            mat.discard

                        newDiscard : Pile
                        newDiscard =
                            { oldDiscard | cards = drawnCard :: oldDiscard.cards }

                        oldDeck : Pile
                        oldDeck =
                            mat.deck

                        newDeck : Pile
                        newDeck =
                            { oldDeck | cards = List.filter ((/=) drawnCard) oldDeck.cards }

                        newMat : Mat
                        newMat =
                            { mat | deck = newDeck, discard = newDiscard }

                        newMats : List Mat
                        newMats =
                            replace mat newMat model.mats
                    in
                    ( { model | mats = newMats, seed = newSeed }, Cmd.none )

        AddCard mat cardType ->
            let
                oldDeck : Pile
                oldDeck =
                    mat.deck

                newDeck : Pile
                newDeck =
                    { oldDeck | cards = Card model.nonce cardType :: oldDeck.cards }

                newMat : Mat
                newMat =
                    { mat | deck = newDeck }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats
            in
            ( { model | mats = newMats, nonce = model.nonce + 1 }, Cmd.none )

        RemoveCard deck mat card ->
            let
                newDeck : Pile
                newDeck =
                    { deck | cards = List.Extra.remove card deck.cards }

                newMat : Mat
                newMat =
                    { mat | deck = newDeck }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats
            in
            ( { model | mats = newMats }, Cmd.none )

        ToggleMatCardEdit mat ->
            let
                newMat : Mat
                newMat =
                    { mat
                        | cardEditState =
                            if mat.cardEditState == Default then
                                Editing

                            else
                                Default
                    }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats
            in
            ( { model | mats = newMats }, Cmd.none )

        ToggleMatNameEdit mat ->
            let
                newMat : Mat
                newMat =
                    { mat
                        | nameEditState =
                            if mat.nameEditState == Default then
                                Editing

                            else
                                Default
                    }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats
            in
            ( { model | mats = newMats }, Cmd.none )

        AddDefaultCards mat ->
            let
                oldDeck : Pile
                oldDeck =
                    mat.deck

                ( newNonce, defaultCards ) =
                    makeDefaultCards model.nonce

                newDeck : Pile
                newDeck =
                    { oldDeck | cards = List.concat [ defaultCards, oldDeck.cards ] }

                newMats : List Mat
                newMats =
                    replace mat { mat | deck = newDeck } model.mats
            in
            ( { model | nonce = newNonce, mats = newMats }, Cmd.none )

        RemoveAllCards mat ->
            let
                oldDeck : Pile
                oldDeck =
                    mat.deck

                oldDiscard : Pile
                oldDiscard =
                    mat.discard

                newMat : Mat
                newMat =
                    { mat
                        | deck = { oldDeck | cards = [] }
                        , discard = { oldDiscard | cards = [] }
                    }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats
            in
            ( { model | mats = newMats }, Cmd.none )

        HandleMatNameInput mat name ->
            let
                newMat : Mat
                newMat =
                    { mat | name = name }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats
            in
            ( { model | mats = newMats }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


cardRow : Mat -> Card -> Html Msg
cardRow mat card =
    let
        removeButton : Html Msg
        removeButton =
            button
                [ onClick (RemoveCard mat.deck mat card)
                , classList [ ( "invisible", mat.cardEditState /= Editing ) ]
                ]
                [ text "-" ]
    in
    li [] [ removeButton, text (stringForCardType card.cardType) ]


renderAddCard : Mat -> CardType -> Html Msg
renderAddCard mat cardType =
    button [ onClick (AddCard mat cardType) ] [ text ("+" ++ stringForCardType cardType) ]


renderMat : Mat -> Html Msg
renderMat mat =
    div []
        [ div []
            (if mat.nameEditState == Editing then
                [ input
                    [ value mat.name
                    , onInput (HandleMatNameInput mat)
                    , onBlur (ToggleMatNameEdit mat)
                    , autofocus True
                    ]
                    []
                ]

             else
                [ text mat.name
                , button
                    [ onClick (ToggleMatNameEdit mat)
                    , class "edit-mat-name"
                    ]
                    [ FeatherIcons.edit
                        |> FeatherIcons.withSize 10
                        |> FeatherIcons.toHtml []
                    ]
                ]
            )
        , div []
            [ button [ onClick (ToggleMatCardEdit mat) ] [ text "Toggle Editing" ] ]
        , div [ classList [ ( "invisible", mat.cardEditState /= Editing ) ] ]
            [ renderAddCard mat Zero
            , renderAddCard mat One
            , renderAddCard mat MinusOne
            , renderAddCard mat Two
            , renderAddCard mat MinusTwo
            , renderAddCard mat Crit
            , renderAddCard mat Null
            , text "|"
            , button [ onClick (AddDefaultCards mat) ] [ text "Add Default Cards" ]
            , button [ onClick (RemoveAllCards mat) ] [ text "Remove All Cards" ]
            ]
        , div []
            [ button [ onClick (Draw mat), disabled (List.isEmpty mat.deck.cards) ] [ text "Draw" ]
            , button [ onClick (Reshuffle mat), disabled (List.isEmpty mat.discard.cards) ] [ text "Reshuffle" ]
            ]
        , div [] [ text "Deck:" ]
        , ul [] (List.map (cardRow mat) mat.deck.cards)
        , div [] [ text "Drawn cards:" ]
        , ul [] (List.map (cardRow mat) mat.discard.cards)
        , hr [] []
        ]


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ button [ onClick AddMat ] [ text "Add Mat" ]
        , hr [] []
        , div []
            (List.map renderMat model.mats)
        ]
