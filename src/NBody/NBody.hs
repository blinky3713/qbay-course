module NBody.NBody where

import           Clash.Prelude
import qualified Prelude       as P

newtype R3 a = R3 (a,a,a) deriving (Eq, Show)

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

acc1
  :: R3 Float
  -> (R3 Float, Float)
  -> R3 Float
acc1 pVec (qVec,mq) = mq/(r*r*r) ⋅ R3 (dx,dy,dz)
  where
    R3 (dx,dy,dz) = qVec - pVec
    r          = sqrt (dx*dx + dy*dy + dz*dz + eps)
    eps = 0.01


router
  :: Num a
  => BodyID
  -> Message a
  -> (Message a, Message a)
  -> (Message a, (Message a, Message a))
router myID reg (fromRing, fromBody) = ( reg' , (toRing,toBody) )
  where
    reg' =
      case fromRing of
        BodyData sendID _ _
          | sendID == myID  ->  UpdateV
          | otherwise       ->  fromRing
        UpdateV ->  UpdateP
        UpdateP ->  Send
        Send ->  addID fromBody
        NoMsg ->  NoMsg
    toRing   = reg
    toBody   = reg

    addID
      :: Num a
      => Message a
      -> Message a
    addID msg =
      case msg of
        BodyData _ p m -> BodyData myID p m
        _              -> msg


body
  :: Float
  -> (R3 Float, R3 Float, R3 Float)
  -> Message Float
  -> ((R3 Float, R3 Float, R3 Float), Message Float)
body m (a, v, p) fromRtr = ((a',v',p') , toRtr)
  where
    -- timestep
    dt  = 21600
    -- gravitational constant
    grC = 6.674e-11
    gdt = dt * grC
    (a',v',p',toRtr) = case fromRtr of
      --  a'                   v'          p'         toRtr
      --  ------------------------------------------------------------
      BodyData _ q mq  -> ( a + acc1 p (q,mq),  v           ,  p       ,  NoMsg          )
      UpdateV          -> ( R3 (0,0,0)          ,  v + gdt⋅a,  p       ,  NoMsg          )
      UpdateP          -> ( R3 (0,0,0)          ,  v        ,  p + dt⋅v,  NoMsg          )
      Send             -> ( R3 (0,0,0)          ,  v        ,  p       ,  BodyData 0 p m )
      NoMsg            -> ( a                   ,  v        ,  p       ,  NoMsg          )

nBody
  :: KnownNat n
  => forall a.
     (Vec n BodyID, Vec n Float)
  -> ( Vec n (Message Float)
     , Vec n (R3 Float, R3 Float, R3 Float)
     )
  -> a
  -> ( ( Vec n (Message Float)
       , Vec n (R3 Float, R3 Float, R3 Float)
       )
     , Float
     )
nBody (_ids, _ms) (routerRegs,bodyRegs) _ = ( (routerRegs',bodyRegs') , 0 )
  where
    routerInputs                = zip (rotateRight toRingAll (1 :: Int)) fromBodyAll
    (routerRegs' , routerOutputs)  = unzip $ zipWith3 router _ids routerRegs routerInputs
    (toRingAll, toBodyAll )  = unzip routerOutputs
    (bodyRegs', fromBodyAll) = unzip $ zipWith3 body _ms bodyRegs toBodyAll

--------------------------------------------------------------------------------

ids :: Vec 4 BodyID
ids  = iterate d4 (+1) 0                -- idents of routers

ms :: Num a => Vec 4 a
ms   = iterate d4 (+1) 10               -- masses of bodies

routerRegs0 :: Vec 4 (Message a)
routerRegs0  = replicate d4 Send           -- initial router registers

bodyRegs0
  :: Vec 4 (R3 Float, R3 Float, R3 Float)
bodyRegs0 =
  replicate d4 ( R3 (1,1,1)      -- initial body registers
               , R3 (1,1,1)
               , R3 (1,1,1)
               )

test :: IO ()
test =
  putStr
     $ unlines
     $ P.map (show . fst)
     $ sim (nBody (ids,ms)) (routerRegs0, bodyRegs0) $ P.replicate 100 ()
  where
    sim
      :: (s -> i -> (s,o))
      -> s
      -> [i]
      -> [s]
    sim _ _ []     = []
    sim f s (i:is) = s : sim f s' is
      where
        (s',_) = f s i


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
    fnc = ring (body <.> router) (zip ms ids)

    run _ s []     = [s]
    run f s (_:is) = s' : run f s' is
            where
              s' = f s

