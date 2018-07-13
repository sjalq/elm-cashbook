port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Transaction =
    { date : Date
    , amount : Double
    , description : String }

type alias Day = 
    { date : Date
    , expectedOpeningBalance : Double
    , recordedOpeningBalance : Double
    , transactions : List Transaction
    , recordedClosingBalance : Double
    , expectedClosingBalance : Double
    }

type alias AllDays = List Day

type alias Model =
    { currentDate : Date
    , days : AllDays
    }

type alias Msg = NextDay
    | PrevDay
    | NextDay
    | RecordOpeningBalance Double
    | RecordClosingBalance Double
    | AddTransaction Transaction
    | RemoveTransaction Int
    | Save

connectionString : String
connectionString = "https://myserver.com"

model = 
    { currentDate = "2010-01-01"
    , days = []
    , expectedClosingBalance = 0
    }

model_openDay model date = 
    { model | currentDate = date }

model_openingBalance model date openingBalance = 
    { model | days = days_changeOpeningBalance date openingBalance days }

list_replace item items filter
    notIn = (List.filter (not filter) days) : item

days_replaceDay newDay days
    list_replace newDay days (/i -> i.date == newDay.date)

update msg model =
    case msg 
        | PrevDay
            model_openDay model (model.currentDate + 1)
        | NextDay
            { model | currentDate = model.currentDate + 1}
        | RecordOpeningBalance openingBalance
            let 
                openDay = find currentDate model.days 
            in
                { model | model.days = addOpeningBalance model.days model.currentDate openingBalance }
        | RecordOpeningBalance openingBalance
            { model | model.days = addOpeningBalance model.days model.currentDate openingBalance }
        | RecordClosingBalance closingBalance
            { model | model.days = addClosingBalance model.days model.currentDate closingBalance }
        | AddTransaction transaction
            { model | model.days = addTransaction model.days model.currentDate transaction }
        | RemoveTransaction transactionId
            { model | model.days = addTransaction model.days model.currentDate transactionId }
        | Save
            _