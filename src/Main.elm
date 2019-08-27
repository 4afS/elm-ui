module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, int, nullable, string, succeed)
import Json.Decode.Pipeline exposing (required)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { input : String
    , userState : UserState
    }


type UserState
    = Init
    | Waiting
    | Loaded User
    | Failed Http.Error


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , userState = Init
      }
    , Cmd.none
    )


type Msg
    = Input String
    | Send
    | Receive (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input s ->
            ( { model
                | input = s
              }
            , Cmd.none
            )

        Send ->
            ( { model
                | userState = Waiting
              }
            , Http.get
                { url = "https://api.github.com/users/" ++ model.input
                , expect = Http.expectJson Receive userDecoder
                }
            )

        Receive (Ok user) ->
            ( { model
                | userState = Loaded user
              }
            , Cmd.none
            )

        Receive (Err e) ->
            ( { model
                | userState = Failed e
              }
            , Cmd.none
            )


type alias Url =
    String


type alias User =
    { login : String
    , avatarUrl : Url
    , htmlUrl : Url
    , followers : Int
    , following : Int
    , publicRepos : Int
    , name : String
    , bio : Maybe String
    }


userDecoder : Decoder User
userDecoder =
    succeed User
        |> required "login" string
        |> required "avatar_url" string
        |> required "html_url" string
        |> required "followers" int
        |> required "following" int
        |> required "public_repos" int
        |> required "name" string
        |> required "bio" (nullable string)


view : Model -> Html Msg
view model =
    div
        [ class "main" ]
        [ inputForm model
        , case model.userState of
            Init ->
                text ""

            Waiting ->
                text "Please wait..."

            Loaded userInfo ->
                showUserInfo userInfo

            Failed e ->
                text <| Debug.toString e
        ]


inputForm : Model -> Html Msg
inputForm model =
    div
        [ class "inputForm" ]
        [ Html.form
            [ onSubmit Send ]
            [ input
                [ class "input"
                , onInput Input
                , autofocus True
                , placeholder "GitHub name"
                , value model.input
                ]
                []
            , button
                [ class "send_button" ]
                [ text "Submit"
                ]
            ]
        ]


showUserInfo : User -> Html Msg
showUserInfo userInfo =
    div
        [ class "user_info" ]
        [ avatar userInfo
        , showUserInfoWithoutAvatar userInfo
        ]


avatar : User -> Html Msg
avatar userInfo =
    img
        [ class "avatar"
        , src userInfo.avatarUrl
        ]
        []


showUserInfoWithoutAvatar : User -> Html Msg
showUserInfoWithoutAvatar userInfo =
    div
        [ class "user_info_without_avatar" ]
        [ nameAndId userInfo
        , bio userInfo
        -- , repos userInfo
        -- , followersAndFollowing userInfo
        ]


nameAndId : User -> Html Msg
nameAndId userInfo =
    div
        [ class "name_id" ]
        [ a
            [ class "name"
            , href userInfo.htmlUrl
            , target "_blank"
            ]
            [ text userInfo.name ]
        , span
            [ class "id" ]
            [ text userInfo.login
            ]
        ]


bio : User -> Html Msg
bio userInfo =
    span
        [ class "bio" ]
        [ case userInfo.bio of
            Just userBio ->
                text userBio

            Nothing ->
                text ""
        ]


repos : User -> Html Msg
repos userInfo =
    div
        [ class "repos" ]
        [ span
            [ class "repos_text" ]
            [ text "Repository" ]
        , span
            [ class "repos_number" ]
            [ text <| String.fromInt userInfo.publicRepos ]
        ]


followersAndFollowing : User -> Html Msg
followersAndFollowing userInfo =
    div
        [ class "followers_following" ]
        [ div
            [ class "followers" ]
            [ span
                [ class "followers_text" ]
                [ text "Followers" ]
            , span
                [ class "followers_number" ]
                [ text <| String.fromInt userInfo.followers ]
            ]
        , div
            [ class "following" ]
            [ span
                [ class "following_text" ]
                [ text "Following" ]
            , span
                [ class "following_number" ]
                [ text <| String.fromInt userInfo.followers ]
            ]
        ]



-- type alias User =
--     { login : String
--     , avatarUrl : Url
--     , htmlUrl : Url
--     , followers : Int
--     , following : Int
--     , publicRepos : Int
--     , name : String
--     , bio : Maybe String
--     }
