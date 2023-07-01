import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Json.Encode as JE
import Json.Decode as JD
import Result.Extra
import Task
import Tuple exposing (first, second)
import Http

-- MODEL

type alias Feeds = Dict String Feed

type Model
  = Loading
  | Loaded { feeds : Feeds, newFeed : NewFeed }

type alias FeedInfo = { name : String, url : String }

type Feed
  = InertFeed { errorMessage : String, info : FeedInfo }
  | EditableFeed { errorMessage : String, newInfo : FeedInfo, oldInfo : FeedInfo }
  | Saving { newInfo : FeedInfo, oldInfo : FeedInfo }
  | DeleteAreYouSure { info : FeedInfo }
  | Deleting { info : FeedInfo }

type NewFeed
  = WaitingForAddNewFeed
  | EditableNewFeed { errorMessage : String, info : FeedInfo }
  | SavingNewFeed { info : FeedInfo }

type alias ServerFeed = { id : String, title : String, origin : String }

encodeFeedInfo : FeedInfo -> JE.Value
encodeFeedInfo {name, url} = JE.object [("title", JE.string name), ("origin", JE.string url)]

decodeFeed : JD.Decoder (String, Feed)
decodeFeed = JD.map3
  (\id title origin -> (id, makeFeed title origin))
  (JD.field "id" JD.string)
  (JD.field "title" JD.string)
  (JD.field "origin" JD.string)

decodeFeeds : JD.Decoder Feeds
decodeFeeds = JD.map Dict.fromList (JD.list decodeFeed)

-- UPDATE

type Msg
  = Reset Feeds
  | ForFeed String FeedMsg
  | ForNewFeed NewFeedMsg
  | DeleteSuccess String
  | AddFeed (String, Feed)

type FeedMsg
  = Edit
  | Cancel
  | Save
  | Delete
  | DeleteError String
  | SaveError String
  | SaveSuccess
  | SetName String
  | SetUrl String
  | Yes
  | No

type FeedEvent
  = SaveEvent FeedInfo
  | DeleteEvent
  | NoEvent

type NewFeedMsg
  = AddNewFeed
  | SaveNewFeed
  | CancelNewFeed
  | SetNewFeedName String
  | SetNewFeedUrl String
  | SaveNewFeedError String

type NewFeedEvent
  = SaveNewFeedEvent FeedInfo
  | NoNewFeedEvent

makeFeed name url = InertFeed {errorMessage="", info={name=name, url=url}}

viewInertFeed : String -> FeedInfo -> Html FeedMsg
viewInertFeed errorMessage info =
  tr []
    [ td [] [text info.name]
    , td [] [text info.url]
    , td []
        [ button [onClick Edit] [text "edit"]
        , button [onClick Delete] [text "delete"]]
    , td [] [text errorMessage]]

type alias ViewFeedConfig msg =
  { onSetName : String -> msg
  , onSetUrl : String -> msg
  , onSave : msg
  , onCancel : msg
  }

viewEditableFeed : ViewFeedConfig msg -> Bool -> String -> FeedInfo -> Html msg
viewEditableFeed config frozen errorMessage info =
  tr []
    [ td [] [div [] [input [value info.name, onInput config.onSetName, readonly frozen] []]]
    , td [] [input [value info.url, onInput config.onSetUrl, readonly frozen] []]
    , td []
        [ button [onClick config.onSave, disabled frozen] [text "save"]
        , button [onClick config.onCancel, disabled frozen] [text "cancel"]]
    , td [] [text errorMessage]]

viewDeleteAreYouSure : FeedInfo -> Html FeedMsg
viewDeleteAreYouSure info =
  tr []
    [ td [colspan 2] [text ("Are you sure you want to delete " ++ info.name ++ "?")]
    , td []
        [ button [onClick Yes] [text "Yes"]
        , button [onClick No] [text "No"]
        ]
    ]

viewDeleting : FeedInfo -> Html FeedMsg
viewDeleting info =
  tr [] [td [colspan 4] [text ("Deleting " ++ info.name)]]


marginRight = style [("margin-right", "1em")]

viewFeedConfig =
  { onSetName = SetName
  , onSetUrl = SetUrl
  , onSave = Save
  , onCancel = Cancel
  }

viewFeed : Feed -> Html FeedMsg
viewFeed feed =
  case feed of
    InertFeed {errorMessage, info} ->
      viewInertFeed errorMessage info
    EditableFeed {errorMessage, newInfo} ->
      viewEditableFeed viewFeedConfig False errorMessage newInfo
    Saving {newInfo} ->
      viewEditableFeed viewFeedConfig True "" newInfo
    DeleteAreYouSure {info} ->
      viewDeleteAreYouSure info
    Deleting {info} ->
      viewDeleting info

updateFeed : Feed -> FeedMsg -> (Feed, FeedEvent)
updateFeed feed msg =
  case feed of
    InertFeed state ->
      case msg of
        Edit ->
          ( EditableFeed {errorMessage = "", newInfo = state.info, oldInfo = state.info}
          , NoEvent )
        Delete ->
          ( DeleteAreYouSure {info = state.info}
          , NoEvent )
        _ -> (feed, NoEvent)
    EditableFeed state ->
      case msg of
        Save ->
          ( Saving {newInfo = state.newInfo, oldInfo = state.oldInfo}
          , SaveEvent state.newInfo )
        Cancel ->
          ( InertFeed {errorMessage = "", info = state.oldInfo}
          , NoEvent )
        SetName name ->
          let newInfo = state.newInfo
          in ( EditableFeed { state | newInfo = { newInfo | name = name } }
             , NoEvent )
        SetUrl url ->
          let newInfo = state.newInfo
          in ( EditableFeed { state | newInfo = { newInfo | url = url } }
             , NoEvent )
        _ -> (feed, NoEvent)
    Saving state ->
      case msg of
        SaveError error ->
          ( EditableFeed {errorMessage = error, newInfo = state.newInfo, oldInfo = state.oldInfo}
          , NoEvent )
        SaveSuccess ->
          ( InertFeed { errorMessage = "", info = state.newInfo }
          , NoEvent )
        _ -> (feed, NoEvent)
    DeleteAreYouSure state ->
      case msg of
        Yes ->
          ( Deleting { info = state.info }
          , DeleteEvent )
        No ->
          ( InertFeed { errorMessage = "", info = state.info }
          , NoEvent )
        _ -> (feed, NoEvent)
    Deleting state ->
      case msg of
        DeleteError error ->
          ( InertFeed { errorMessage = error, info = state.info }
          , NoEvent )
        _ -> (feed, NoEvent)

updateNewFeed : NewFeed -> NewFeedMsg -> (NewFeed, NewFeedEvent)
updateNewFeed newFeed msg =
  case newFeed of
    WaitingForAddNewFeed ->
      case msg of
        AddNewFeed ->
          (EditableNewFeed {errorMessage = "", info = {name="", url=""}}, NoNewFeedEvent)
        _ ->
          (newFeed, NoNewFeedEvent)
    EditableNewFeed state ->
      case msg of
        SaveNewFeed ->
          (SavingNewFeed {info=state.info}, SaveNewFeedEvent state.info)
        CancelNewFeed ->
          (WaitingForAddNewFeed, NoNewFeedEvent)
        SetNewFeedName name ->
          let info = state.info
          in (EditableNewFeed {state | info = { info | name = name }}, NoNewFeedEvent)
        SetNewFeedUrl url ->
          let info = state.info
          in (EditableNewFeed {state | info = { info | url = url }}, NoNewFeedEvent)
        _ ->
          (newFeed, NoNewFeedEvent)
    SavingNewFeed state ->
      case msg of
        SaveNewFeedError error ->
          (EditableNewFeed {errorMessage = error, info = state.info}, NoNewFeedEvent)
        _ ->
          (newFeed, NoNewFeedEvent)

viewFeeds : Feeds -> List (Html Msg)
viewFeeds feeds =
  (List.map
    (\idFeed -> Html.map (ForFeed (first idFeed)) (viewFeed (second idFeed)))
    (Dict.toList feeds))

viewNewFeedConfig =
  { onSetName = SetNewFeedName
  , onSetUrl = SetNewFeedUrl
  , onSave = SaveNewFeed
  , onCancel = CancelNewFeed
  }

viewNewFeed : NewFeed -> Html NewFeedMsg
viewNewFeed mfeed =
  case mfeed of
    WaitingForAddNewFeed ->
      tr [] [td [colspan 3] [button [onClick AddNewFeed] [text "Add new feed"]]]
    EditableNewFeed {errorMessage, info}->
      viewEditableFeed viewNewFeedConfig False errorMessage info
    SavingNewFeed {info} ->
      viewEditableFeed viewNewFeedConfig True "" info

view : Model -> Html Msg
view model =
  case model of
    Loading -> text "Loading"
    Loaded state ->
      table [] <|
        viewFeeds state.feeds ++
        [Html.map ForNewFeed (viewNewFeed state.newFeed)]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Loading ->
      case msg of
        Reset feeds ->
          (Loaded {feeds=feeds, newFeed=WaitingForAddNewFeed}, Cmd.none)
        _ ->
          (model, Cmd.none)
    Loaded state ->
      case msg of
        ForFeed id subMsg ->
          case Dict.get id state.feeds of
            Nothing -> (Loaded state, Cmd.none)
            Just feed ->
              let (newFeed, event) = updateFeed feed subMsg
              in (Loaded {state | feeds = Dict.insert id newFeed state.feeds},
                  feedEventToCmd id event)
        DeleteSuccess id ->
          (Loaded { state | feeds = Dict.remove id state.feeds }, Cmd.none)
        Reset feeds ->
          (Loaded {feeds=feeds, newFeed=WaitingForAddNewFeed}, Cmd.none)
        ForNewFeed subMsg ->
          let (newNewFeed, event) = updateNewFeed state.newFeed subMsg
          in (Loaded {state | newFeed = newNewFeed}, newFeedEventToCmd event)
        AddFeed (id, feed) ->
          (Loaded
            { state |
                feeds = Dict.insert id feed state.feeds,
                newFeed = WaitingForAddNewFeed },
           Cmd.none)

--urlPrefix = "http://makeitso.no-ip.org:3000"
urlPrefix = "http://localhost:3000"
feedsUrl = urlPrefix ++ "/feeds"
feedUrl id = feedsUrl ++ "/" ++ id

getFeedsCmd =
  Http.send
    (Result.Extra.unpack
      (\_ -> Reset Dict.empty)
      Reset)
    (Http.get feedsUrl decodeFeeds)

newFeedEventToCmd : NewFeedEvent -> Cmd Msg
newFeedEventToCmd event =
  case event of
    SaveNewFeedEvent info ->
      (Http.send
        (Result.Extra.unpack
          (toString >> SaveNewFeedError >> ForNewFeed)
          AddFeed)
        (Http.post
          feedsUrl
          (Http.jsonBody (encodeFeedInfo info))
          decodeFeed))
    NoNewFeedEvent ->
      Cmd.none

feedEventToCmd : String -> FeedEvent -> Cmd Msg
feedEventToCmd id event =
  case event of
    SaveEvent info ->
      Http.send
        (Result.Extra.unpack
          (toString >> SaveError >> ForFeed id)
          (\_ -> ForFeed id SaveSuccess))
        (Http.request
          { method = "PUT"
          , headers = []
          , url = feedUrl id
          , body = Http.jsonBody (encodeFeedInfo info)
          , expect = Http.expectString
          , timeout = Nothing
          , withCredentials = False
          })
    DeleteEvent ->
      Http.send
        (Result.Extra.unpack
          (toString >> DeleteError >> ForFeed id)
          (\_ -> DeleteSuccess id))
        (Http.request
          { method = "DELETE"
          , headers = []
          , url = feedUrl id
          , body = Http.emptyBody
          , expect = Http.expectString
          , timeout = Nothing
          , withCredentials = False
          })
    NoEvent ->
      Cmd.none

initialModel = Loading

main = Html.program
  { init = (initialModel, getFeedsCmd)
  , update = update
  , subscriptions = \_ -> Sub.none
  , view = view
  }
