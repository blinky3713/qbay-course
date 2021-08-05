module NBody.Signal where

import           NBody.NBody   (BodyID, Message (..), R3 (..), updateBody)
import qualified Clash.Signal.Delayed.Bundle as DBundle
import           Clash.Prelude


{-
updateBodyP
  :: Float -- body mass
  -> Signal System (R3 Float, R3 Float, R3 Float) -- current (acceleration, velocity, position)
  -> Signal System (Message Float) -- incoming message from the router
  -> Signal System ((R3 Float, R3 Float, R3 Float), Message Float) -- new (acceleration, velocity, position), message to router
updateBodyP m avp messageFromRouter =
  updateBody m <$> avp <*> messageFromRouter
-}

updateBodyP
  :: forall d.
     KnownNat d
  => SNat d
  -> Float -- body mass
  -> Signal System (R3 Float, R3 Float, R3 Float) -- current (acceleration, velocity, position)
  -> Signal System (Message Float) -- incoming message from the router
  -> DSignal System d ((R3 Float, R3 Float, R3 Float), Message Float) -- new (acceleration, velocity, position), message to router
updateBodyP = undefined

-- Given a bodyID and message currenly stored in the register
-- and
routeP
  :: forall a d.
     KnownNat d
  => BodyID
  -> DSignal System d (Message a)
  -> DSignal System d (Message a, Message a)
  -> DSignal System d (Message a, (Message a, Message a))
routeP myID currentRegisterVal fromRingAndBody =
    DBundle.bundle ( f <$> fromRingAndBody , toRingAndBody)
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

    toRing :: DSignal System d (Message a)
    toRing   = currentRegisterVal

    toBody :: DSignal System d (Message a)
    toBody   = currentRegisterVal

    toRingAndBody :: DSignal System d (Message a, Message a)
    toRingAndBody = DBundle.bundle (toRing, toBody)

    addMyID :: Message a -> Message a
    addMyID msg =
      case msg of
        BodyData _ p m -> BodyData myID p m
        _              -> msg

updateNBodiesP
  :: forall n d.
     KnownNat n
  => KnownNat d
  => SNat d
  -> ( Vec n BodyID
     , Vec n Float
     ) -- global env of all bodyIDs and masses
  -> Signal System
       ( Vec n (Message Float)
       , Vec n (R3 Float, R3 Float, R3 Float)
       ) -- current router state and body states
  -> DSignal System d
       ( Vec n (Message Float)
       , Vec n (R3 Float, R3 Float, R3 Float)
       ) -- new router state and body states
updateNBodiesP d (_ids, _ms) routerAndBodyRegs =
    DBundle.bundle (routerRegs', bodyRegs')

  where

    routerRegs :: Signal System (Vec n (Message Float))
    bodyRegs :: Signal System (Vec n (R3 Float, R3 Float, R3 Float))
    (routerRegs, bodyRegs) = unbundle routerAndBodyRegs

    routerInputs :: DSignal System d (Vec n (Message Float, Message Float))
    routerInputs =
      zip . flip rotateRight (1 :: Int) <$>
        toRingAll <*>
        fromBodyAll

    routerRegs' :: DSignal System d (Vec n (Message Float))
    routerOutputs :: DSignal System d (Vec n (Message Float, Message Float))
    (routerRegs', routerOutputs) =
      DBundle.unbundle $ fmap unzip $ DBundle.bundle $
        zipWith3
          ($)
          (routeP <$> _ids)
          (DBundle.unbundle $ delayedI undefined $ fromSignal routerRegs)
          (DBundle.unbundle routerInputs)

    toRingAll :: DSignal System d (Vec n (Message Float))
    toBodyAll :: DSignal System d (Vec n (Message Float))
    (toRingAll, toBodyAll)  = DBundle.unbundle $ fmap unzip routerOutputs

    bodyRegs' :: DSignal System d (Vec n (R3 Float, R3 Float, R3 Float))
    fromBodyAll :: DSignal System d (Vec n (Message Float))
    (bodyRegs', fromBodyAll) = feedback $
      DBundle.unbundle $ fmap unzip $ DBundle.bundle $
        zipWith3
          ($)
          (updateBodyP d <$> _ms)
          (unbundle bodyRegs)
          (DBundle.unbundle toBodyAll)


--------------------------------------------------------------------------------

foldrp
  :: forall dom a b d n.
     HiddenClockResetEnable dom
  => KnownNat n
  => NFDataX b
  => NFDataX a
  => KnownNat d
  => (a -> b -> b)
  -- ^ f
  -> b
  -- ^ default ouput
  -> a
  -- ^ default input
  -> Vec n (DSignal dom d a)
  -- ^ Vector to fold over
  -> DSignal dom d b
  -- ^ Start value
  -> DSignal dom (d + n) b
foldrp _ bStart _ Nil b = delayedI @n bStart b
foldrp f bStart  aStart (Cons a as) b =
  let b' = delayedI @1 bStart (f <$> a <*> b)
  in foldrp f bStart aStart (delayedI @1 aStart <$> as) b'
