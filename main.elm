import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model = 
  { time : Int
  , seconds : Int
  , paused : Bool
  , inputValue : String
  }

initialValue : Int
initialValue = 20

init : (Model, Cmd Msg)
init =
  ( Model initialValue 0 True ( toString initialValue ), Cmd.none )

-- UPDATE

type Msg
  = Tick Time 
    | Toggle 
    | SetTime String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model_ =
  case msg of
    Tick newTime ->
      if model_.time > 0 || model_.seconds > 0
        then if model_.seconds == 0
        then ( { model_ | time = model_.time-1
          , seconds = 59 
          }, Cmd.none ) 
        else ( { model_ | seconds = model_.seconds-1 }, Cmd.none ) 
      else ( { model_ | time = initialValue
        , seconds = 0
        , paused = True
        }, Cmd.none )
    SetTime newTime ->
      let resetValue = ( Result.withDefault 0 ( String.toInt newTime ) )
      in ( { model_ | time = resetValue
        , seconds = 0
        , paused = True
        , inputValue = toString resetValue 
        }, Cmd.none )
    Toggle ->
      ( { model_ | paused = not model_.paused } , Cmd.none )
  
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused 
  then Sub.none
  else Time.every second Tick

-- VIEW

view : Model -> Html Msg
view model_ = div [ Html.Attributes.style 
    [ ( "margin", "auto" )
    , ( "margin-top", "100px" )
    , ( "width", "50%" )
    , ( "padding", "10px" )
    , ( "text-align", "center" )
    ] ]
  [ div []
    [ input [ placeholder "Minutes?"
      , Html.Attributes.type_ "number"
      , defaultValue ( toString initialValue )
      , onInput SetTime
      , hidden ( not ( checkInitial model_ ) )
      , Html.Attributes.style [ ( "margin-right", "10px" ) ]
      ] []
    , button [ onClick Toggle, Html.Attributes.style 
      [ ( "margin-right", "10px" ) ] ] [ Html.text <| buttonText model_ ]
    , button [ onClick ( SetTime model_.inputValue )
      , hidden ( checkInitial model_ )
      ] [ Html.text "Stop" ]
  ] 
  , div [ Html.Attributes.style 
    [ ( "margin-top", "20px" )
    , ( "font-size", "100px" )
    ] ] []
  , svg [ Svg.Attributes.width "320", Svg.Attributes.height "120", viewBox "0 0 320 120" ]
    [ rect [ x "10", y "10", Svg.Attributes.width "300", Svg.Attributes.height "100", rx "15", ry "15" ] []
    , text_ [ x "160", y "75", textAnchor "middle", Svg.Attributes.style "fill: red", fontSize "45" ] [ Svg.text ( ( toString model_.time ) ++ " : " ++ ( toString model_.seconds ) ) ]
    ]
  ]

buttonText : Model -> String
buttonText model_ =
  if model_.paused
  then if checkInitial model_
    then "Start"
    else "Resume"
  else "Pause"

checkInitial : Model -> Bool
checkInitial model_ =
  if ( ( toString model_.time ) == model_.inputValue ) && model_.paused
  then True
  else False
  