module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Dom
import FeatherIcons
import Html exposing (Html, button, div, hr, input, li, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, id, value)
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


getMatId : Id -> String
getMatId id =
    "mat-" ++ String.fromInt id



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


type Card
    = StandardCard { id : Id, cardType : CardType }
    | CustomCard { id : Id, description : String }


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
    , customCardText : String
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
    , [ StandardCard { id = nonce + 0, cardType = Zero }
      , StandardCard { id = nonce + 1, cardType = One }
      , StandardCard { id = nonce + 2, cardType = MinusOne }
      , StandardCard { id = nonce + 3, cardType = Two }
      , StandardCard { id = nonce + 4, cardType = MinusTwo }
      , StandardCard { id = nonce + 5, cardType = Crit }
      , StandardCard { id = nonce + 6, cardType = Null }
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
    , customCardText = ""
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
    = NoOp
    | GenerateSeed Time.Posix
    | AddMat
      -- mat operations
    | Reshuffle Mat
    | Draw Mat
    | AddStandardCard Mat CardType
    | AddCustomCard Mat
    | RemoveCard Pile Mat Card
    | ToggleMatCardEdit Mat
    | ToggleMatNameEdit Mat
    | AddDefaultCards Mat
    | RemoveAllCards Mat
    | HandleMatNameInput Mat String
    | HandleCustomCardInput Mat String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
                    , customCardText = ""
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

        AddStandardCard mat cardType ->
            let
                oldDeck : Pile
                oldDeck =
                    mat.deck

                newDeck : Pile
                newDeck =
                    { oldDeck | cards = StandardCard { id = model.nonce, cardType = cardType } :: oldDeck.cards }

                newMat : Mat
                newMat =
                    { mat | deck = newDeck }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats
            in
            ( { model | mats = newMats, nonce = model.nonce + 1 }, Cmd.none )

        AddCustomCard mat ->
            let
                oldDeck : Pile
                oldDeck =
                    mat.deck

                newDeck : Pile
                newDeck =
                    { oldDeck | cards = CustomCard { id = model.nonce, description = mat.customCardText } :: oldDeck.cards }

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
            ( { model | mats = newMats }, focusMatNameInput mat.id )

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

        HandleCustomCardInput mat text ->
            let
                newMat : Mat
                newMat =
                    { mat | customCardText = text }

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

        label : String
        label =
            case card of
                StandardCard { cardType } ->
                    stringForCardType cardType

                CustomCard { description } ->
                    description
    in
    li [] [ removeButton, text label ]


renderAddCustomCard : Mat -> Html Msg
renderAddCustomCard mat =
    span []
        [ button [ onClick (AddCustomCard mat) ] [ text "+Custom Card" ]
        , input [ onInput (HandleCustomCardInput mat) ] []
        ]


renderAddCard : Mat -> CardType -> Html Msg
renderAddCard mat cardType =
    button [ onClick (AddStandardCard mat cardType) ] [ text ("+" ++ stringForCardType cardType) ]


focusMatNameInput : Id -> Cmd Msg
focusMatNameInput id =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus <| getMatId id)


renderMat : Mat -> Html Msg
renderMat mat =
    div []
        [ div [ class "mat-name" ]
            (if mat.nameEditState == Editing then
                [ input
                    [ value mat.name
                    , onInput (HandleMatNameInput mat)
                    , onBlur (ToggleMatNameEdit mat)
                    , id (getMatId mat.id)
                    ]
                    []
                ]

             else
                [ span [] [ text mat.name ]
                , button
                    [ onClick (ToggleMatNameEdit mat) ]
                    [ FeatherIcons.edit
                        |> FeatherIcons.withSize 24
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
            , renderAddCustomCard mat
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
