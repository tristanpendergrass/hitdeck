port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Dom
import FeatherIcons
import Html exposing (Html, button, div, h2, hr, img, input, li, span, text, ul)
import Html.Attributes exposing (alt, class, classList, disabled, id, src, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Random
import Random.List exposing (shuffle)
import Task
import Time


main : Program D.Value Model Msg
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


cardShouldReshuffle : Card -> Bool
cardShouldReshuffle card =
    case card of
        CustomCard _ ->
            True

        StandardCard { cardType } ->
            cardType /= Blessing && cardType /= Curse



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
    | Blessing
    | Curse


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


makeDefaultCards : Nonce -> ( Nonce, List Card )
makeDefaultCards nonce =
    ( nonce + 21
    , [ StandardCard { id = nonce + 19, cardType = Crit }
      , StandardCard { id = nonce + 20, cardType = Null }
      , StandardCard { id = nonce + 1, cardType = Zero }
      , StandardCard { id = nonce + 2, cardType = Zero }
      , StandardCard { id = nonce + 3, cardType = Zero }
      , StandardCard { id = nonce + 4, cardType = Zero }
      , StandardCard { id = nonce + 5, cardType = Zero }
      , StandardCard { id = nonce + 6, cardType = Zero }
      , StandardCard { id = nonce + 7, cardType = One }
      , StandardCard { id = nonce + 8, cardType = One }
      , StandardCard { id = nonce + 9, cardType = One }
      , StandardCard { id = nonce + 10, cardType = One }
      , StandardCard { id = nonce + 11, cardType = One }
      , StandardCard { id = nonce + 12, cardType = MinusOne }
      , StandardCard { id = nonce + 13, cardType = MinusOne }
      , StandardCard { id = nonce + 14, cardType = MinusOne }
      , StandardCard { id = nonce + 15, cardType = MinusOne }
      , StandardCard { id = nonce + 16, cardType = MinusOne }
      , StandardCard { id = nonce + 17, cardType = Two }
      , StandardCard { id = nonce + 18, cardType = MinusTwo }
      ]
    )


defaultMat : String -> Mat
defaultMat initialName =
    let
        ( _, defaultCards ) =
            makeDefaultCards 0
    in
    { id = 0
    , deck = { id = 50, cards = defaultCards }
    , discard = { id = 51, cards = [] }
    , cardEditState = Default
    , nameEditState = Default
    , name = initialName
    , customCardText = ""
    }


init : D.Value -> ( Model, Cmd Msg )
init localStorageData =
    let
        nonce : Int
        nonce =
            case D.decodeValue (D.field "nonce" D.int) localStorageData of
                Ok val ->
                    val

                Err _ ->
                    1000
    in
    ( { mats = [ defaultMat "default" ]
      , nonce = nonce
      , seed = Random.initialSeed 0
      }
    , Task.perform GenerateSeed Time.now
    )



-- UPDATE


port sendToLocalStorage : E.Value -> Cmd msg


encodeModel : Model -> E.Value
encodeModel model =
    E.object [ ( "nonce", E.int model.nonce ) ]


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
                cardsToShuffle : List Card
                cardsToShuffle =
                    [ mat.deck.cards, mat.discard.cards ]
                        |> List.concat
                        |> List.filter cardShouldReshuffle

                ( shuffledCards, newSeed ) =
                    Random.step (shuffle cardsToShuffle) model.seed

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
            ( { model | mats = newMats }, sendToLocalStorage <| encodeModel model )

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


labelForCardType : CardType -> String
labelForCardType cardType =
    case cardType of
        Zero ->
            "+0"

        One ->
            "+1"

        MinusOne ->
            "-1"

        Two ->
            "+2"

        MinusTwo ->
            "-2"

        Crit ->
            "Crit"

        Null ->
            "Null"

        Blessing ->
            "Blessing"

        Curse ->
            "Curse"


srcForCardType : CardType -> String
srcForCardType cardType =
    case cardType of
        Zero ->
            "attack_modifiers/0.png"

        One ->
            "attack_modifiers/1.png"

        MinusOne ->
            "attack_modifiers/minus_1.png"

        Two ->
            "attack_modifiers/2.png"

        MinusTwo ->
            "attack_modifiers/minus_2.png"

        Crit ->
            "attack_modifiers/crit.png"

        Null ->
            "attack_modifiers/null.png"

        Blessing ->
            "attack_modifiers/blessing.png"

        Curse ->
            "attack_modifiers/curse.png"


cardTypeClass : Card -> String
cardTypeClass card =
    case card of
        CustomCard _ ->
            "custom"

        StandardCard { cardType } ->
            case cardType of
                Crit ->
                    "crit"

                Null ->
                    "null"

                Blessing ->
                    "blessing"

                Curse ->
                    "curse"

                _ ->
                    "normal"


renderCard : Mat -> Card -> Html Msg
renderCard mat card =
    let
        removeButton : Html Msg
        removeButton =
            button
                [ onClick (RemoveCard mat.deck mat card)
                , classList [ ( "remove-button", True ), ( "invisible", mat.cardEditState /= Editing ) ]
                ]
                [ text "-" ]

        displayCard : Html Msg
        displayCard =
            case card of
                StandardCard { cardType } ->
                    div [ class "standard-card-container" ]
                        [ img [ class "card standard background", src "attack_modifiers/blank.jpg", alt "Card Background" ] []
                        , img [ class "card standard foreground", src <| srcForCardType cardType, alt <| labelForCardType cardType ] []
                        ]

                CustomCard { description } ->
                    div [ class "card custom" ] [ text description ]
    in
    li [ class "card-container" ]
        [ displayCard
        , removeButton
        ]


renderAddCustomCard : Mat -> Html Msg
renderAddCustomCard mat =
    span
        [ classList [ ( "invisible", mat.cardEditState /= Editing ) ] ]
        [ button
            [ onClick (AddCustomCard mat) ]
            [ text "+Custom Card" ]
        , input [ onInput (HandleCustomCardInput mat) ] []
        ]


renderAddCard : Mat -> CardType -> Html Msg
renderAddCard mat cardType =
    div []
        [ button
            [ onClick (AddStandardCard mat cardType)
            , classList [ ( "invisible", mat.cardEditState /= Editing ) ]
            ]
            [ text ("Add " ++ labelForCardType cardType) ]
        ]


focusMatNameInput : Id -> Cmd Msg
focusMatNameInput id =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus <| getMatId id)


renderMat : Mat -> Html Msg
renderMat mat =
    div [ class "mat" ]
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
        , div [ class "mat-container" ]
            [ div [ class "buttons-pane" ]
                [ div [] [ button [ onClick (ToggleMatCardEdit mat) ] [ text "Toggle Editing" ] ]
                , renderAddCard mat Zero
                , renderAddCard mat One
                , renderAddCard mat MinusOne
                , renderAddCard mat Two
                , renderAddCard mat MinusTwo
                , renderAddCard mat Crit
                , renderAddCard mat Null
                , renderAddCard mat Blessing
                , renderAddCard mat Curse
                , renderAddCustomCard mat
                , div [ classList [ ( "invisible", mat.cardEditState /= Editing ) ] ] [ button [ onClick (AddDefaultCards mat) ] [ text "Add Default Cards" ] ]
                , div [ classList [ ( "invisible", mat.cardEditState /= Editing ) ] ] [ button [ class "warn", onClick (RemoveAllCards mat) ] [ text "Remove All Cards" ] ]
                ]
            , div [ class "deck-pane" ]
                [ div [ class "pane-header" ]
                    [ h2 [] [ text "Deck" ]
                    , button [ onClick (Draw mat), disabled (List.isEmpty mat.deck.cards) ]
                        [ FeatherIcons.zap
                            |> FeatherIcons.withSize 16
                            |> FeatherIcons.toHtml []
                        , text "Draw"
                        ]
                    ]
                , ul [] (List.map (renderCard mat) mat.deck.cards)
                ]
            , div [ class "discard-pane" ]
                [ div [ class "pane-header" ]
                    [ h2 [] [ text "Discard" ]
                    , button [ onClick (Reshuffle mat), disabled (List.isEmpty mat.discard.cards) ]
                        [ FeatherIcons.shuffle
                            |> FeatherIcons.withSize 16
                            |> FeatherIcons.toHtml []
                        , text "Reshuffle"
                        ]
                    ]
                , ul [] (List.map (renderCard mat) mat.discard.cards)
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ button [ onClick AddMat ] [ text "Add Mat" ]
        , hr [] []
        , div []
            (List.map renderMat model.mats)
        ]
