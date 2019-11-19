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
    = StandardCard
        { id : Id
        , cardType : CardType
        }
    | CustomCard
        { id : Id
        , description : String
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

        cardTypeDecoder : D.Decoder CardType
        cardTypeDecoder =
            D.map
                (\cardType ->
                    case cardType of
                        "Zero" ->
                            Zero

                        "One" ->
                            One

                        "MinusOne" ->
                            MinusOne

                        "Two" ->
                            Two

                        "MinusTwo" ->
                            MinusTwo

                        "Crit" ->
                            Crit

                        "Null" ->
                            Null

                        "Blessing" ->
                            Blessing

                        "Curse" ->
                            Curse

                        _ ->
                            Zero
                )
                D.string

        standardCardDecoder : D.Decoder Card
        standardCardDecoder =
            D.map2 (\id cardType -> StandardCard { id = id, cardType = cardType })
                (D.field "id" D.int)
                (D.field "cardType" cardTypeDecoder)

        customCardDecoder : D.Decoder Card
        customCardDecoder =
            D.map2 (\id description -> CustomCard { id = id, description = description })
                (D.field "id" D.int)
                (D.field "description" D.string)

        cardDecoder : D.Decoder Card
        cardDecoder =
            D.oneOf
                [ standardCardDecoder, customCardDecoder ]

        cardsDecoder : D.Decoder (List Card)
        cardsDecoder =
            D.list cardDecoder

        pileDecoder : D.Decoder Pile
        pileDecoder =
            D.map2 Pile
                (D.field "id" D.int)
                (D.field "cards" cardsDecoder)

        editStateDecoder : D.Decoder EditState
        editStateDecoder =
            D.map
                (\editState ->
                    if editState == "Editing" then
                        Editing

                    else
                        Default
                )
                D.string

        matDecoder : D.Decoder Mat
        matDecoder =
            D.map7 Mat
                (D.field "id" D.int)
                (D.field "deck" pileDecoder)
                (D.field "discard" pileDecoder)
                (D.field "cardEditState" editStateDecoder)
                (D.field "nameEditState" editStateDecoder)
                (D.field "name" D.string)
                (D.field "customCardText" D.string)

        mats : List Mat
        mats =
            case D.decodeValue (D.field "mats" (D.list matDecoder)) localStorageData of
                Ok val ->
                    val

                Err _ ->
                    []
    in
    ( { mats = mats
      , nonce = nonce
      , seed = Random.initialSeed 0
      }
    , Task.perform GenerateSeed Time.now
    )



-- UPDATE


port sendToLocalStorage : E.Value -> Cmd msg


encodeModel : Model -> E.Value
encodeModel model =
    let
        encodeCard : Card -> E.Value
        encodeCard card =
            case card of
                StandardCard { id, cardType } ->
                    let
                        cardTypeString : String
                        cardTypeString =
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

                                Blessing ->
                                    "Blessing"

                                Curse ->
                                    "Curse"
                    in
                    E.object
                        [ ( "id", E.int id )
                        , ( "cardType", E.string cardTypeString )
                        ]

                CustomCard { id, description } ->
                    E.object
                        [ ( "id", E.int id )
                        , ( "description", E.string description )
                        ]

        encodePile : Pile -> E.Value
        encodePile pile =
            E.object
                [ ( "id", E.int pile.id )
                , ( "cards", E.list encodeCard pile.cards )
                ]

        encodeEditState : EditState -> E.Value
        encodeEditState editState =
            case editState of
                Default ->
                    E.string "Default"

                Editing ->
                    E.string "Editing"

        encodeMat : Mat -> E.Value
        encodeMat mat =
            E.object
                [ ( "id", E.int mat.id )
                , ( "deck", encodePile mat.deck )
                , ( "discard", encodePile mat.discard )
                , ( "cardEditState", encodeEditState mat.cardEditState )
                , ( "nameEditState", encodeEditState Default )
                , ( "name", E.string mat.name )
                , ( "customCardText", E.string mat.customCardText )
                ]
    in
    E.object
        [ ( "nonce", E.int model.nonce )
        , ( "mats", E.list encodeMat model.mats )
        ]


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

                newModel : Model
                newModel =
                    { model | mats = model.mats ++ [ newMat ], nonce = model.nonce + 3 }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        Reshuffle mat ->
            let
                cardsToShuffle : List Card
                cardsToShuffle =
                    [ mat.deck.cards, List.filter cardShouldReshuffle mat.discard.cards ]
                        |> List.concat

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

                newModel : Model
                newModel =
                    { model | mats = newMats, seed = newSeed }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

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

                        newModel : Model
                        newModel =
                            { model | mats = newMats, seed = newSeed }
                    in
                    ( newModel, sendToLocalStorage <| encodeModel newModel )

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

                newModel : Model
                newModel =
                    { model | mats = newMats, nonce = model.nonce + 1 }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

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

                newModel : Model
                newModel =
                    { model | mats = newMats, nonce = model.nonce + 1 }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

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

                newModel : Model
                newModel =
                    { model | mats = newMats }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

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

                newModel : Model
                newModel =
                    { model | mats = newMats }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

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

                newModel : Model
                newModel =
                    { model | mats = newMats }
            in
            ( newModel
            , Cmd.batch
                [ sendToLocalStorage <| encodeModel newModel
                , focusMatNameInput mat.id
                ]
            )

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

                newModel : Model
                newModel =
                    { model | nonce = newNonce, mats = newMats }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

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

                newModel : Model
                newModel =
                    { model | mats = newMats }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        HandleMatNameInput mat name ->
            let
                newMat : Mat
                newMat =
                    { mat | name = name }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats

                newModel : Model
                newModel =
                    { model | mats = newMats }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        HandleCustomCardInput mat text ->
            let
                newMat : Mat
                newMat =
                    { mat | customCardText = text }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats

                newModel : Model
                newModel =
                    { model | mats = newMats }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )



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


renderCard : Card -> Html Msg
renderCard card =
    let
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
        [ displayCard ]


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


type alias CardGroup =
    ( Card, List Card )


groupCards : List Card -> List CardGroup
groupCards cards =
    let
        insertCardOrCreateGroup : Card -> List CardGroup -> List CardGroup
        insertCardOrCreateGroup card groups =
            let
                ( cardWasFound, updatedGroups ) =
                    List.foldr
                        (\group ( cardIsFound, currentGroups ) ->
                            let
                                cardNotFoundResult =
                                    ( cardIsFound, group :: currentGroups )
                            in
                            case ( card, Tuple.first group ) of
                                ( StandardCard cardData, StandardCard groupCardData ) ->
                                    if cardData.cardType == groupCardData.cardType then
                                        ( True, ( Tuple.first group, card :: Tuple.second group ) :: currentGroups )

                                    else
                                        cardNotFoundResult

                                ( CustomCard cardData, CustomCard groupCardData ) ->
                                    if cardData.description == groupCardData.description then
                                        ( True, ( Tuple.first group, card :: Tuple.second group ) :: currentGroups )

                                    else
                                        cardNotFoundResult

                                _ ->
                                    cardNotFoundResult
                        )
                        ( False, [] )
                        groups
            in
            if cardWasFound then
                updatedGroups

            else
                ( card, [ card ] ) :: groups
    in
    cards
        |> List.foldl insertCardOrCreateGroup []
        |> List.sortBy (Tuple.second >> List.length >> (*) -1)


renderCardGroup : Mat -> CardGroup -> Html Msg
renderCardGroup mat ( card, cardGroup ) =
    let
        shownCards : Int
        shownCards =
            min
                5
                (List.length cardGroup)
    in
    div [ class "card-group-container" ]
        [ div [ class "card-group-cards" ]
            (card
                |> List.repeat shownCards
                |> List.map renderCard
            )
        , button
            [ onClick (RemoveCard mat.deck mat card)
            , classList [ ( "remove-button", True ), ( "invisible", mat.cardEditState /= Editing ) ]
            ]
            [ text "-" ]
        , div
            [ class "card-group-number" ]
            [ FeatherIcons.x
                |> FeatherIcons.withSize 12
                |> FeatherIcons.toHtml []
            , text <| String.fromInt (List.length cardGroup)
            ]
        ]


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
                , ul []
                    (mat.deck.cards
                        |> groupCards
                        |> List.map (renderCardGroup mat)
                    )
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
                , ul [] (List.map renderCard mat.discard.cards)
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
