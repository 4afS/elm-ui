module Update exposing (update)


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
                | input = ""
                , userState = Waiting
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
                | userState = Faild e
              }
            , Cmd.none
            )
