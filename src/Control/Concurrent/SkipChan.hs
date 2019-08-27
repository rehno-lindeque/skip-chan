module Control.Concurrent.SkipChan where

import Control.Concurrent.MVar

data SkipChan a = SkipChan (MVar (a, [MVar ()])) (MVar ())

{-# inline newSkipChan #-}
newSkipChan :: IO (SkipChan a)
newSkipChan = do
  sem <- newEmptyMVar
  main <- newMVar (undefined, [sem])
  return (SkipChan main sem)

{-# inline writeSkipChan #-}
writeSkipChan :: SkipChan a -> a -> IO ()
writeSkipChan (SkipChan main _) a = do
  (_, sems) <- takeMVar main
  putMVar main (a, [])
  mapM_ (\sem -> putMVar sem ()) sems

{-# inline readSkipChan #-}
readSkipChan :: SkipChan a -> IO a
readSkipChan (SkipChan main sem) = do
  takeMVar sem
  (a, sems) <- takeMVar main
  putMVar main (a, sem:sems)
  return a

{-# inline dupSkipChan #-}
dupSkipChan :: SkipChan a -> IO (SkipChan a)
dupSkipChan (SkipChan main _) = do
  sem <- newEmptyMVar
  (a, sems) <- takeMVar main
  putMVar main (a, sem:sems)
  return (SkipChan main sem)

