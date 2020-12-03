module App where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Int

data Action = Increment | Decrement

css = HP.class_ <<< HH.ClassName

-- The HTML type says that the Component manipulates the DOM, the
-- the `query` type represents how parent components communicate
-- with child components, the `input` type specifies the data that
-- the component accepts, the `output` type specifies the data that
-- the component sends to the parent, and `m` being used to run
-- effects.
component :: forall query input output m. H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

-- The type of the input does not matter as we're not handling it.
initialState :: forall input. input -> State
initialState _ = 0

-- ComponentHTML is a more specialized type of HTML, specifically
-- meant for use in components. `()` is used in place to describe
-- child components with `m` being a monadic type for use in effects.
render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ css "hero is-primary is-fullheight" ]
  [ HH.div [ css "hero-body" ]
    [ HH.div [ css "container is-fluid has-text-centered" ]
      [ HH.p [ css "is-size-1-tablet" ] [ HH.text $ "Count: " <> show state ]
      , HH.div [ css "buttons has-addons is-centered" ]
        [ HH.button [ HE.onClick \_ -> Just Decrement, css "button is-danger is-inverted" ] [ HH.text "-" ]
        , HH.button [ HE.onClick \_ -> Just Increment, css "button is-success is-inverted" ] [ HH.text "+" ]
        ]
      ]
    ]
  ]

-- HalogenM is the main Monad Transformer type used for handling
-- component state while also composing it together with other
-- monads through `m`. It has to know the type of the `State`
-- being encapsulated, the type of the `Action` being received,
-- any child components signified by `()`, and the type of the
-- `output` to be communicated to any parent component if any.
-- This is also typically parameterized by `Unit` as state
-- modifications do not emit any resulting values.
handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Decrement ->
    H.modify_ \state -> state - 1

  Increment ->
    H.modify_ \state -> state + 1
