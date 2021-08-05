module NBody.NBody where

import           Clash.Prelude
import qualified Prelude       as P

newtype R3 a = R3 (a,a,a) deriving (Eq)

instance Show a => Show (R3 a) where
  show (R3 v) = show v

instance Num a => Num (R3 a) where
  R3 (x,y,z) + R3 (x',y',z')  =  R3 (x+x', y+y', z+z')
  R3 (x,y,z) - R3 (x',y',z')  =  R3 (x-x', y-y', z-z')
  R3 (x,y,z) * R3 (x',y',z')  =  R3 (x*x', y*y', z*z')
  abs _                 =  P.error "No abs def for R3"
  signum _              =  P.error "No signum def for R3"
  fromInteger _         =  P.error "No fromInteger def for R3"

(⋅)
  :: Num a
  => a
  -> R3 a
  -> R3 a
a ⋅ R3 (x,y,z) = R3 (a*x, a*y, a*z)

infixl 7 ⋅

type BodyID = Int

data Message a
  = BodyData BodyID (R3 a) a
  | UpdateV
  | UpdateP
  | Send
  | NoMsg
  deriving (Eq, Show)

-- Calculate the acceleration of body P due to
-- the gravitational force exerted by body Q P
acc1
  :: R3 Float
  -> (R3 Float, Float)
  -> R3 Float
acc1 pVec (qVec,mq) = (grC * mq)/(r*r*r) ⋅ R3 (dx,dy,dz)
  where
    -- gravitational constant
    grC = 6.674e-11
    R3 (dx,dy,dz) = qVec - pVec
    r          = sqrt (dx*dx + dy*dy + dz*dz + eps)
    eps = 0.01

-- Given a bodyID and message currenly stored in the register
-- and
route
  :: BodyID
  -> Message a
  -> (Message a, Message a)
  -> (Message a, (Message a, Message a))
route myID currentRegisterVal (fromRing, fromBody) = ( newRegisterVal , (toRing, toBody) )
  where
    newRegisterVal =
      case fromRing of
        BodyData sendID _ _
          | sendID == myID  ->  UpdateV
          | otherwise       ->  fromRing
        UpdateV ->  UpdateP
        UpdateP ->  Send
        Send ->  addMyID fromBody
        NoMsg ->  NoMsg
    toRing   = currentRegisterVal
    toBody   = currentRegisterVal

    addMyID msg =
      case msg of
        BodyData _ p m -> BodyData myID p m
        _              -> msg

newtype Router n a = Router { unRouter :: Vec n (Message a) }


updateBody
  :: Float -- body mass
  -> (R3 Float, R3 Float, R3 Float) -- current (acceleration, velocity, position)
  -> Message Float -- incoming message from the router
  -> ((R3 Float, R3 Float, R3 Float), Message Float) -- new (acceleration, velocity, position), message to router
updateBody m (a, v, p) messageFromRouter = ((a',v',p') , messageToRouter)
  where
    -- timestep
    dt  = 21600
    (a', v', p', messageToRouter) = case messageFromRouter of
      --  a'                   v'          p'         toRtr
      --  ------------------------------------------------------------
      BodyData _ q mq  -> ( a + acc1 p (q,mq)   , v        , p       , NoMsg          )
      UpdateV          -> ( R3 (0,0,0)          , v + dt⋅a , p       , NoMsg          )
      UpdateP          -> ( R3 (0,0,0)          , v        , p + dt⋅v, NoMsg          )
      Send             -> ( R3 (0,0,0)          , v        , p       , BodyData 0 p m )
      NoMsg            -> ( a                   , v        , p       , NoMsg          )

updateNBodies
  :: forall n.
     KnownNat n
  => ( Vec n BodyID
     , Vec n Float
     ) -- global env of all bodyIDs and masses
  -> ( Vec n (Message Float)
     , Vec n (R3 Float, R3 Float, R3 Float)
     ) -- current router state and body states
  -> ( Vec n (Message Float)
     , Vec n (R3 Float, R3 Float, R3 Float)
     ) -- new router state and body states
updateNBodies (_ids, _ms) (routerRegs, bodyRegs) = (routerRegs', bodyRegs')
  where
    routerInputs :: Vec n (Message Float, Message Float)
    routerInputs = zip (rotateRight toRingAll (1 :: Int)) fromBodyAll

    routerRegs' :: Vec n (Message Float)
    routerOutputs :: Vec n (Message Float, Message Float)
    (routerRegs', routerOutputs) = unzip $ zipWith3 route _ids routerRegs routerInputs

    toRingAll :: Vec n (Message Float)
    toBodyAll :: Vec n (Message Float)
    (toRingAll, toBodyAll )  = unzip routerOutputs

    bodyRegs' :: Vec n (R3 Float, R3 Float, R3 Float)
    fromBodyAll :: Vec n (Message Float)
    (bodyRegs', fromBodyAll) = unzip $ zipWith3 updateBody _ms bodyRegs toBodyAll

--------------------------------------------------------------------------------
-- Test input
--------------------------------------------------------------------------------

ids :: Vec 4 BodyID
ids  = iterate d4 (+1) 1                -- idents of routers

ms :: Num a => Vec 4 a
ms   = iterate d4 (+5) 10               -- masses of bodies

routerRegs0 :: Vec 4 (Message a)
routerRegs0  = replicate d4 Send           -- initial router registers

bodyRegs0
  :: Vec 4 (R3 Float, R3 Float, R3 Float)
bodyRegs0 =
  (R3 (1,1,1), R3 (1,1,1), R3 (1,0,0)) :>
    (R3 (1,1,1), R3 (1,1,1), R3 (0,1,0)) :>
    (R3 (1,1,1), R3 (1,1,1), R3 (0,0,1)) :>
    (R3 (1,1,1), R3 (1,1,1), R3 (-1,-1,-1)) :>
    Nil


test :: IO ()
test =
  putStr
     $ unlines
     $ P.map (show . fst)
     $ sim (updateNBodies (ids,ms)) (routerRegs0, bodyRegs0) $ P.replicate 100 ()
  where
    -- simulates a clock cycle
    sim
      :: (s -> s)
      -> s
      -> [i]
      -> [s]
    sim _ _ []     = []
    sim f s (_:is) = s : sim f s' is
      where
        s' = f s


{-second definition of ring structure
 - Used: composition of router and body
 - TODO: correct position of ms and ids; now confusing
-}

(<.>)
  :: (t -> t1 -> t2 -> (a, b))
  -> (t3 -> t4 -> (a1, b) -> (b1, (b2, t2)))
  -> (t, t3)
  -> (t1, t4)
  -> a1
  -> ((a, b1), b2)
f <.> g = \(pf,pg) (_sf,sg) inp ->
  let (sf', x      ) = f pf _sf y
      (sg', (out,y)) = g pg sg (inp,x)
  in ((sf',sg'), out)

ring
  :: (a -> b -> c -> (a1, c))
  -> Vec 4 a
  -> Vec 4 b
  -> Vec 4 a1
ring fnc ps ss = ss'
  where
    (ss',outs) = unzip $ zipWith3 fnc ps ss inps
    inps       = rotateRight outs (1 :: Int)

test2 :: IO ()
test2 =
  putStr
    $ unlines
    $ P.map (show . (!! (3 :: Int)))
    $ run fnc (zip bodyRegs0 routerRegs0) $ P.replicate 100 ()
  where
    fnc = ring (updateBody <.> route) (zip ms ids)

    run _ s []     = [s]
    run f s (_:is) = s' : run f s' is
            where
              s' = f s

