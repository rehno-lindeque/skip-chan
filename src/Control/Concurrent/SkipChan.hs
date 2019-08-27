module Control.Concurrent.SkipChan where

import Control.Concurrent.MVar
import Control.Exception.Base

data SkipChan a = SkipChan (MVar (a, [MVar ()])) (MVar ())

{-# inline newSkipChan #-}
newSkipChan :: a -> IO (SkipChan a)
newSkipChan a = do
  sem <- newMVar ()
  main <- newMVar (a, [])
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
  sem <- newMVar ()
  return (SkipChan main sem)

{-# inline extendSkipChan_ #-}
extendSkipChan_ :: SkipChan a -> (a -> IO a) -> IO ()
extendSkipChan_ (SkipChan main _) io =
  mask $ \restore -> do
    (a, sems) <- takeMVar main
    a' <- restore (io a) `onException` putMVar main (a,sems)
    putMVar main (a',[])
    mapM_ (\sem -> putMVar sem ()) sems

{-# inline extendSkipChan #-}
extendSkipChan :: SkipChan a -> (a -> IO (a, b)) -> IO b
extendSkipChan (SkipChan main _) io = do
  mask $ \restore -> do
    (a, sems) <- takeMVar main
    (a', b) <- restore (io a >>= evaluate) `onException` putMVar main (a,sems)
    putMVar main (a',[])
    mapM_ (\sem -> putMVar sem ()) sems
    return b

{-# inline extendSkipChanMasked_ #-}
extendSkipChanMasked_ :: SkipChan a -> (a -> IO a) -> IO ()
extendSkipChanMasked_ (SkipChan main _) io =
  mask_ $ do
    (a, sems) <- takeMVar main
    a' <- io a `onException` putMVar main (a,sems)
    putMVar main (a',[])
    mapM_ (\sem -> putMVar sem ()) sems

{-# inline extendSkipChanMasked #-}
extendSkipChanMasked :: SkipChan a -> (a -> IO (a, b)) -> IO b
extendSkipChanMasked (SkipChan main _) io = do
  mask_ $ do
    (a, sems) <- takeMVar main
    (a', b) <- (io a >>= evaluate) `onException` putMVar main (a,sems)
    putMVar main (a',[])
    mapM_ (\sem -> putMVar sem ()) sems
    return b
