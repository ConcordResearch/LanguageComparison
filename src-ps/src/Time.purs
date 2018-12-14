module Time (now) where

import Effect (Effect, foreachE)

foreign import now :: Effect Number
