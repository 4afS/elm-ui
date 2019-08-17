module Init exposing (init)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , userState = Init
      }
    , Cmd.none
    )
