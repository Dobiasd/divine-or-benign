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
    Random.list roundCount (Random.int 0 (documentCount language))
        |> Random.generate RandomNumbers


type State
    = Asking
    | ShowingAnswer Bool


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
                    , state = ShowingAnswer True
                  }
                , Cmd.none
                )
            else
                ( { model
                    | state = ShowingAnswer False
                  }
                , Cmd.none
                )

        BenignClick ->
            if isCurrentDocumentDivine model then
                ( { model
                    | state = ShowingAnswer False
                  }
                , Cmd.none
                )
            else
                ( { model
                    | points = model.points + 1
                    , state = ShowingAnswer True
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
            , cmdGetRandomNumbers model.language
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

                ShowingAnswer result ->
                    viewShowingAnswer result model
            ]
        , hr [] []
        , showExplanation model.language
        , hr [] []
        , showFooter
        ]


showPoints : Language -> Int -> Int -> Html Msg
showPoints language points rounds =
    div [ class "points panel panel-default" ]
        [ textPoints language
            ++ ": "
            ++ toString points
            ++ "/"
            ++ toString rounds
            |> text
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



-- todo: progres bar
-- todo: make end more noticable, show score in big


viewShowingAnswer : Bool -> Model -> Html Msg
viewShowingAnswer result model =
    let
        notDoneYet =
            model.round < roundCount

        buttonToShow =
            if notDoneYet then
                button [ class "button btn btn-default btn-lg", onClick ContinueClick ] [ text (continueText model.language) ]
            else
                div []
                    [ button [ class "button btn btn-default btn-lg", onClick RestartClick ] [ text (restartText model.language) ] ]

        ( resultString, resultClass ) =
            if result then
                ( textCorrect model.language, "result alert alert-success" )
            else
                ( textWrong model.language, "result alert alert-danger" )

        resultHtml =
            div [ class resultClass ]
                [ div [ class "panel panel-default questiontext" ]
                    [ text resultString ]
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



-- todo: ads


showFooter : Html Msg
showFooter =
    footer [ class "footer" ]
        [ text "Copyright © 2016 Tobias Hermann. All rights reserved. Contact: info (at) editgym.com"
        ]


showDocument : Bool -> Document -> Html Msg
showDocument withSource document =
    let
        textString =
            if withSource then
                document.content ++ "\n\n" ++ document.passage
            else
                document.content ++ "\n\n" ++ "-"
    in
        div [ class "documentwrapper" ]
            [ div [ class "document" ] [ Markdown.toHtml [] textString ]
            ]


showButtons : Language -> Html Msg
showButtons language =
    div []
        [ div [ class "alert alert-info question" ]
            [ div [ class "panel panel-default questiontext" ]
                [ text (questionText language) ]
            ]
        , div [ class "buttons" ]
            [ button [ class "btn btn-default btn-lg button", onClick DivineClick ] [ text (divineButtonText language) ]
            , text (orText language)
            , button [ class "btn btn-default btn-lg button", onClick BenignClick ] [ text (benignButtonText language) ]
            ]
        ]


showHeader : Html Msg
showHeader =
    div [ class "header panel panel-default" ]
        [ div [ class "panel-body" ]
            [ img [ class "headerimage", src "img/creation_of_adam.jpg" ] []
            , text "vs."
            , img [ class "headerimage", src "img/troll_net.jpg" ] []
            ]
        ]


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

## Can you distinguish God from a recurrent neural network?

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

## Kannst du den Schöpfer von einem rekurrenten neuronalen Netz unterscheiden?

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
            "Gott"

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
            "Was denkst du? Ist dieser Text"

        otherwise ->
            "What do you think? Is this text"


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


textCorrect : Language -> String
textCorrect language =
    case language of
        German ->
            "Deine Antwort war richtig."

        otherwise ->
            "Your answer was Correct."


textWrong : Language -> String
textWrong language =
    case language of
        German ->
            "Deine Antwort war falsch."

        otherwise ->
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
