port module DivineOrBenign exposing (..)

import Database exposing (Document, TextSource, documentsEnglish, documentsGerman)
import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Random
import String


port languagePort : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    languagePort (parseLanguage >> LanguageSelect)


parseLanguage : String -> Language
parseLanguage str =
    case str of
        "de" ->
            German

        otherwise ->
            English


main =
    program
        { init = initModelAndCommands
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


initModelAndCommands : ( Model, Cmd Msg )
initModelAndCommands =
    ( defaultModel, cmdGetRandomNumbers English )


roundCount : Int
roundCount =
    10


documentCount : Language -> Int
documentCount language =
    List.length (documents language)


cmdGetRandomNumbers : Language -> Cmd Msg
cmdGetRandomNumbers language =
    Random.list roundCount (Random.int 0 (documentCount language - 1))
        |> Random.generate RandomNumbers


type State
    = Asking
    | ShowingAnswer Bool Bool


type alias Model =
    { documents : List Document
    , points : Int
    , round : Int
    , language : Language
    , state : State
    }


defaultModel : Model
defaultModel =
    { documents = []
    , points = 0
    , round = 1
    , language = English
    , state = Asking
    }


type Language
    = English
    | German


type Msg
    = NoOp
    | RandomNumbers (List Int)
    | LanguageSelect Language
    | DivineClick
    | BenignClick
    | ContinueClick
    | RestartClick


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NoOp ->
            ( model, Cmd.none )

        DivineClick ->
            if isCurrentDocumentDivine model then
                ( { model
                    | points = model.points + 1
                    , state = ShowingAnswer True True
                  }
                , Cmd.none
                )
            else
                ( { model
                    | state = ShowingAnswer False False
                  }
                , Cmd.none
                )

        BenignClick ->
            if not <| isCurrentDocumentDivine model then
                ( { model
                    | points = model.points + 1
                    , state = ShowingAnswer True False
                  }
                , Cmd.none
                )
            else
                ( { model
                    | state = ShowingAnswer False True
                  }
                , Cmd.none
                )

        ContinueClick ->
            ( { model
                | state = Asking
                , round = model.round + 1
              }
            , Cmd.none
            )

        RestartClick ->
            ( { model
                | round = 1
                , points = 0
                , state = Asking
              }
            , cmdGetRandomNumbers model.language
            )

        LanguageSelect newLanguage ->
            ( { model
                | language = newLanguage
              }
            , cmdGetRandomNumbers newLanguage
            )

        RandomNumbers nums ->
            ( { model
                | documents = getRandomDocuments model.language nums
              }
            , Cmd.none
            )


nthElement : List a -> Int -> Maybe a
nthElement xs idx =
    List.drop idx xs |> List.head


getRandomDocuments : Language -> List Int -> List Document
getRandomDocuments language chosenIdxs =
    chosenIdxs
        |> List.filterMap (nthElement (documents language))


view : Model -> Html Msg
view model =
    div [ class "mainwrapper" ]
        [ div [ class "main" ]
            [ showHeader
            , case model.state of
                Asking ->
                    viewAsking model

                ShowingAnswer wasCorrect wasDivine ->
                    viewShowingAnswer wasCorrect wasDivine model
            ]
        , hr [] []
        , showExplanation model.language
        , hr [] []
        , showOtherLanguages model.language
        , hr [] []
        , showFooter
        ]


showPoints : Language -> Int -> Int -> Html Msg
showPoints language points rounds =
    let
        wrongCount =
            rounds - points

        correctPercentage =
            round (100 * toFloat points / toFloat roundCount)

        wrongPercentage =
            round (100 * toFloat wrongCount / toFloat roundCount)

        progressBar =
            div [ class "progress" ]
                [ div
                    [ class "progress-bar progress-bar-success progress-bar-striped"
                    , style [ ( "width", toString correctPercentage ++ "%" ) ]
                    ]
                    []
                , div
                    [ class "progress-bar progress-bar-danger progress-bar-striped"
                    , style [ ( "width", toString wrongPercentage ++ "%" ) ]
                    ]
                    []
                ]
    in
        div [ class "points panel panel-default" ]
            [ textPoints language
                ++ ": "
                ++ toString points
                ++ "/"
                ++ toString rounds
                |> text
            , progressBar
            ]


currentDocument : Model -> Document
currentDocument model =
    Maybe.withDefault
        (Document Database.RNN
            "currentDocument"
            "ERROR: currentDocument failed."
        )
        (nthElement model.documents (model.round - 1))


isCurrentDocumentDivine : Model -> Bool
isCurrentDocumentDivine model =
    let
        document =
            currentDocument model
    in
        document.source == Database.God


viewAsking : Model -> Html Msg
viewAsking model =
    div [ class "jumbotron jumbotronmod" ]
        [ currentDocument model |> showDocument False
        , showButtons model.language
        , showPoints model.language model.points (model.round - 1)
        ]


viewShowingAnswer : Bool -> Bool -> Model -> Html Msg
viewShowingAnswer wasCorrect wasDivine model =
    let
        notDoneYet =
            model.round < roundCount

        buttonToShow =
            if notDoneYet then
                button [ class "btn btn-default btn-lg button", onClick ContinueClick ] [ text (continueText model.language) ]
            else
                button [ class "btn btn-default btn-lg button", onClick RestartClick ] [ text (restartText model.language) ]

        doneDiv =
            div [ class "panel panel-default donepanel" ]
                [ div [ class "donetext" ]
                    [ 100
                        * model.points
                        // roundCount
                        |> toString
                        |> \x ->
                            resultText model.language
                                ++ x
                                ++ "%"
                                |> text
                    ]
                ]

        ( resultString, resultClass, doneDivToShow ) =
            case ( wasCorrect, notDoneYet ) of
                ( True, True ) ->
                    ( textCorrect model.language wasDivine
                    , "result alert alert-success"
                    , div [] []
                    )

                ( False, True ) ->
                    ( textWrong model.language wasDivine
                    , "result alert alert-danger"
                    , div [] []
                    )

                ( True, False ) ->
                    ( textCorrect model.language wasDivine
                    , "result alert alert-success"
                    , doneDiv
                    )

                ( False, False ) ->
                    ( textWrong model.language wasDivine
                    , "result alert alert-danger"
                    , doneDiv
                    )

        resultHtml =
            div []
                [ div [ class resultClass ]
                    [ div [ class "panel panel-default questiontext" ]
                        [ text resultString ]
                    ]
                , doneDivToShow
                ]
    in
        div [ class "jumbotron jumbotronmod" ]
            [ currentDocument model
                |> showDocument True
            , div
                []
                [ resultHtml
                , div [ class "buttons" ] [ buttonToShow ]
                ]
            , showPoints model.language model.points model.round
            ]


showOtherLanguages : Language -> Html Msg
showOtherLanguages language =
    let
        ( str, lang, lnk ) =
            case language of
                German ->
                    ( "Andere Sprachen: ", "Englisch", "index.html?lang=en" )

                otherwise ->
                    ( "other languages: ", "German", "index.html?lang=de" )
    in
        div [ class "otherlanguages" ]
            [ text str
            , a [ href lnk ] [ text lang ]
            ]


showFooter : Html Msg
showFooter =
    footer [ class "footer" ]
        [ text "Copyright © 2016 Tobias Hermann. All rights reserved. Contact: info (at) editgym.com"
        ]


showDocument : Bool -> Document -> Html Msg
showDocument withSource document =
    let
        textString =
            document.content

        passageString =
            if withSource then
                document.passage
            else
                "✝?"
    in
        div [ id "textdocument", class "document" ]
            [ p [] [ text textString ]
            , p [] [ text passageString ]
            ]


showButtons : Language -> Html Msg
showButtons language =
    div []
        [ div [ class "alert alert-info question" ]
            [ div [ class "panel panel-default questiontext" ]
                [ text (questionText language) ]
            ]
        , div [ class "buttons" ]
            [ button [ class "btn btn-default btn-lg button leftbutton", onClick DivineClick ] [ text (divineButtonText language) ]
            , text (orText language)
            , button [ class "btn btn-default btn-lg button rightbutton", onClick BenignClick ] [ text (benignButtonText language) ]
            ]
        ]


showHeader : Html Msg
showHeader =
    div []
        [ img [ class "banner img-responsive", src "img/banner.jpg" ] [] ]


showExplanation : Language -> Html Msg
showExplanation language =
    case language of
        German ->
            showExplanationGerman

        otherwise ->
            showExplanationEnglish


showExplanationEnglish : Html Msg
showExplanationEnglish =
    div [ class "explanation" ]
        [ Markdown.toHtml [] """

### Can you distinguish God from a recurrent neural network?

Assign the shown text to its source, i.e.
[the Bible](https://en.wikipedia.org/wiki/King_James_Version)
or a [RNN](https://en.wikipedia.org/wiki/Recurrent_neural_network)
generating more or less random nonsense.
A RNN is a mathematical model suitable to process text.
The one used here was trained on text from the Bible to be able to match its style.

"""
        ]


showExplanationGerman : Html Msg
showExplanationGerman =
    div [ class "explanation" ]
        [ Markdown.toHtml [] """

### Kannst du den Schöpfer von einem rekurrenten neuronalen Netz unterscheiden?

Ordne dem gezeigten Text seine Quelle zu, d.h. entweder
[die Bibel](https://de.wikipedia.org/wiki/Lutherbibel)
oder ein [RNN](https://de.wikipedia.org/wiki/Rekurrentes_neuronales_Netz),
dass mehr oder weniger zufälligen Unfug generiert.
Ein RNN ist ein mathematisches Model, dass sich zur Textverarbeitung eignet.
Das hier benutzte wurde mit Texten aus der Bibel trainiert, um ihren Stil imitieren zu können.

"""
        ]


divineButtonText : Language -> String
divineButtonText language =
    case language of
        German ->
            "von Gott"

        otherwise ->
            "divine"


benignButtonText : Language -> String
benignButtonText language =
    case language of
        German ->
            "Schrott"

        otherwise ->
            "benign"


orText : Language -> String
orText language =
    case language of
        German ->
            "oder"

        otherwise ->
            "or"


questionText : Language -> String
questionText language =
    case language of
        German ->
            "Was ist dieser Text?"

        otherwise ->
            "What is this text?"


continueText : Language -> String
continueText language =
    case language of
        German ->
            "nächste Runde"

        otherwise ->
            "next round"


restartText : Language -> String
restartText language =
    case language of
        German ->
            "nochmal"

        otherwise ->
            "once again"


textCorrect : Language -> Bool -> String
textCorrect language wasDivine =
    case ( language, wasDivine ) of
        ( German, True ) ->
            "Deine Antwort war richtig."

        ( German, False ) ->
            "Deine Antwort war richtig."

        ( otherwise, True ) ->
            "Your answer was correct."

        ( otherwise, False ) ->
            "Your answer was correct."


textWrong : Language -> Bool -> String
textWrong language wasDivine =
    case ( language, wasDivine ) of
        ( German, True ) ->
            "Falsch. Das ist biblisch."

        ( German, False ) ->
            "Deine Antwort war falsch."

        ( otherwise, True ) ->
            "Wrong. This is scriptural."

        ( otherwise, False ) ->
            "Your answer was wrong."


textPoints : Language -> String
textPoints language =
    case language of
        German ->
            "deine Punkte"

        otherwise ->
            "your score"


documents : Language -> List Document
documents language =
    case language of
        German ->
            documentsGerman

        otherwise ->
            documentsEnglish


resultText : Language -> String
resultText language =
    case language of
        German ->
            "Fertig. Dein Ergebnis: "

        otherwise ->
            "Done. Your score: "
