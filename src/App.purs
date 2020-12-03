module App where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Action = Increment | Decrement

css = HP.class_ <<< HH.ClassName

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState _ = 0

render state =
  HH.div [ css "hero is-primary is-fullheight" ]
  [ HH.div [ css "hero-body" ]
    [ HH.div [ css "container is-fluid has-text-centered" ]
      [ HH.div [ css "is-flex is-justify-content-center is-align-items-center" ]
        [ HH.button [ HE.onClick \_ -> Just Decrement, css "button is-danger is-inverted" ] [ HH.text "-" ]
        , HH.p [ css "is-size-1-tablet" ] [ HH.text $ "Hello, World: " <> show state ]
        , HH.button [ HE.onClick \_ -> Just Increment, css "button is-success is-inverted" ] [ HH.text "+" ]
        ]
      ]
    ]
  ]

handleAction = case _ of
  Decrement ->
    H.modify_ \state -> state - 1

  Increment ->
    H.modify_ \state -> state + 1
