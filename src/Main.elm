module Main exposing (main)

import Browser
import Element exposing (Color, Element)
import Element.Font as Font
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
    toHtml <|
        Element.column
            [ Element.padding 10
            ]
            [ Element.html <| inputForm model
            , case model.userState of
                Init ->
                    Element.text ""

                Waiting ->
                    Element.text "Please wait..."

                Loaded user ->
                    userInfo user

                Failed e ->
                    Element.text <| Debug.toString e
            ]


toHtml : Element Msg -> Html Msg
toHtml =
    Element.layout []


inputForm : Model -> Html Msg
inputForm model =
    div
        []
        [ Html.form
            [ onSubmit Send ]
            [ input
                [ onInput Input
                , autofocus True
                , placeholder "GitHub name"
                , value model.input
                ]
                []
            , button
                []
                [ text "Submit"
                ]
            ]
        ]


userInfo : User -> Element Msg
userInfo user =
    Element.row
        [ Element.spacing 40 ]
        [ avatar user
        , Element.column
            [ Element.spacing 15 ]
          <|
            flap
                [ nameAndId
                , bio
                , repos
                , followrsAndFollowing
                ]
                user
        ]


flap : List (a -> b) -> a -> List b
flap fs a =
    case fs of
        [] ->
            []

        f :: xs ->
            f a :: flap xs a


avatar : User -> Element Msg
avatar user =
    Element.image
        []
        { src = user.avatarUrl
        , description = ""
        }


nameAndId : User -> Element Msg
nameAndId user =
    Element.row
        [ Element.spacing 10
        ]
        [ Element.el
            [ Font.size 60 ]
          <|
            Element.text user.name
        , Element.el
            [ Font.color gray
            , Font.size 35
            , Element.centerY
            ]
          <|
            Element.text user.login
        ]


bio : User -> Element Msg
bio user =
    case user.bio of
        Just s ->
            Element.el
                [ Font.color gray
                , Font.size 15
                ]
            <|
                Element.text s

        Nothing ->
            Element.text ""


repos : User -> Element Msg
repos user =
    elementAndNumber "Repository" user.publicRepos


followrsAndFollowing : User -> Element Msg
followrsAndFollowing user =
    Element.row
        [ Element.spacing 20 ]
        [ elementAndNumber "Followers" user.followers
        , elementAndNumber "Following" user.following
        ]


elementAndNumber : String -> Int -> Element Msg
elementAndNumber s n =
    Element.row
        [ Element.spacing 10 ]
        [ Element.el
            [ Font.color black
            , Font.size 20
            ]
          <|
            Element.text s
        , Element.el
            [ Font.color darkGray
            , Font.size 15
            , Element.alignBottom
            ]
          <|
            Element.text <|
                String.fromInt n
        ]


black : Color
black =
    Element.rgb255 19 10 9


darkGray : Color
darkGray =
    Element.rgb255 66 65 56
