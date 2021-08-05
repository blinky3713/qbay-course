module NBody.Signal where

import           NBody.NBody   (BodyID, Message (..), R3 (..), updateBody)

import           Clash.Prelude


updateBodyP
  :: Float -- body mass
  -> Signal System (R3 Float, R3 Float, R3 Float) -- current (acceleration, velocity, position)
  -> Signal System (Message Float) -- incoming message from the router
  -> Signal System ((R3 Float, R3 Float, R3 Float), Message Float) -- new (acceleration, velocity, position), message to router
updateBodyP m avp messageFromRouter =
  updateBody m <$> avp <*> messageFromRouter

-- Given a bodyID and message currenly stored in the register
-- and
routeP
  :: forall a.
     BodyID
  -> Signal System (Message a)
  -> Signal System (Message a, Message a)
  -> Signal System (Message a, (Message a, Message a))
routeP myID currentRegisterVal fromRingAndBody =
    bundle ( f <$> fromRingAndBody , toRingAndBody)
  where
    f :: (Message a, Message a) -> Message a
    f (fromRing, fromBody) = case fromRing of
       BodyData sendID _ _
         | sendID == myID  ->  UpdateV
         | otherwise       ->  fromRing
       UpdateV ->  UpdateP
       UpdateP ->  Send
       Send ->  addMyID fromBody
       NoMsg ->  NoMsg

    toRing :: Signal System (Message a)
    toRing   = currentRegisterVal

    toBody :: Signal System (Message a)
    toBody   = currentRegisterVal

    toRingAndBody :: Signal System (Message a, Message a)
    toRingAndBody = bundle (toRing, toBody)

    addMyID :: Message a -> Message a
    addMyID msg =
      case msg of
        BodyData _ p m -> BodyData myID p m
        _              -> msg

updateNBodiesP
  :: forall n.
     KnownNat n
  => ( Vec n BodyID
     , Vec n Float
     ) -- global env of all bodyIDs and masses
  -> Signal System
       ( Vec n (Message Float)
       , Vec n (R3 Float, R3 Float, R3 Float)
       ) -- current router state and body states
  -> Signal System
       ( Vec n (Message Float)
       , Vec n (R3 Float, R3 Float, R3 Float)
       ) -- new router state and body states
updateNBodiesP (_ids, _ms) routerAndBodyRegs = bundle (routerRegs', bodyRegs')

  where

    routerRegs :: Signal System (Vec n (Message Float))
    bodyRegs :: Signal System (Vec n (R3 Float, R3 Float, R3 Float))
    (routerRegs, bodyRegs) = unbundle routerAndBodyRegs

    routerInputs :: Signal System (Vec n (Message Float, Message Float))
    routerInputs = zip <$> (flip rotateRight (1 :: Int) <$> toRingAll) <*> fromBodyAll

    routerRegs' :: Signal System (Vec n (Message Float))
    routerOutputs :: Signal System (Vec n (Message Float, Message Float))
    (routerRegs', routerOutputs) =
      unbundle $ fmap unzip $ bundle $
        zipWith3 ($) (routeP <$> _ids) (unbundle routerRegs) (unbundle routerInputs)

    toRingAll :: Signal System (Vec n (Message Float))
    toBodyAll :: Signal System (Vec n (Message Float))
    (toRingAll, toBodyAll)  = unbundle $ fmap unzip routerOutputs

    bodyRegs' :: Signal System (Vec n (R3 Float, R3 Float, R3 Float))
    fromBodyAll :: Signal System (Vec n (Message Float))
    (bodyRegs', fromBodyAll) =
      unbundle $ fmap unzip $ bundle $
        zipWith3 ($) (updateBodyP <$> _ms) (unbundle bodyRegs) (unbundle toBodyAll)
