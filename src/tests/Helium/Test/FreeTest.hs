{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: documentation based on
-- https://github.com/roberth/uu-cs-orc/blob/master/timekeeper/test/FreeTest.hs

module Helium.Test.FreeTest
  ( -- * Running tests
    runInteractions
  , Interact
  , handlePart
  , TestControl
  , runTestControl_
  , runTestControl
  , runTestControl'
  , TestExitStatus(..)
  
    -- * Interacting with the test subject
  , expect
  , stub
  , stubs
  , collect
  , result
  , interact
  
    -- * Controlling the test
  , fulfilled
  , unexpected
  , expecting
  , peekEvent
  , failure
  
    -- * Other
  , Deferable(..)
  , defer
  , now
  , spy
  , runWriterIO
  , Show1Q(..)
  , runIO
  , module Control.Monad.Freer
  ) where
import           Prelude hiding(interact)
import           Control.Arrow(first)
import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Writer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Internal
import           Data.IORef

-- | Reply with a value or defer the reply.
--
-- What will be defered to depends on the context.
data Deferable a = Now a
                 | Defer
instance Functor Deferable where
  fmap f (Now a) = Now (f a)
  fmap f Defer = Defer

-- | Lets you handle part of the events without consuming the entire effect.
--
-- This is useful when some events are somehow not interesting or can be expressed
-- in terms of other effects.
handlePart :: (forall b. f b -> Eff (f ': r) (Deferable b))
  -> Eff (f ': r) a
  -> Eff (f ': r) a
handlePart f (E a q) =
  case decomp a of -- relevant?
    Right a' -> -- relevant
      do fa <- f a'
         case fa of -- handled or not handled?
           Now x -> qComp q (handlePart f) x
           Defer -> E a (tsingleton (qComp q (handlePart f)))
    Left _ -> -- not relevant - re-emit event and keep going
      E a (tsingleton (qComp q (handlePart f)))
handlePart _ (Val x) = Val x

-- | An effect for terminating the test when the test requirements have been
-- fulfilled, or not.
type TestControl = Exc TestExitStatus
data TestExitStatus = Fulfilled | TestFailure String

-- | Runs a test, letting it terminate early, as appropriate.
--
-- For more control over the interpretation of early termination events,
-- use @runTestControl@.
runTestControl_ :: Eff (TestControl ': r) () -> Eff r ()
runTestControl_ = runTestControl fail (return ())

-- | Runs a test, requiring a value to be produced
--
-- For more control over the interpretation of early termination events,
-- use @runTestControl@.
runTestControl' :: Eff (TestControl ': r) a -> Eff r a
runTestControl' = runTestControl fail (fail "Test was 'fulfilled' without providing an expected value.")

-- | Runs a test, letting it terminate early, as appropriate.
runTestControl :: (String -> Eff r a) -- * On failure
               -> Eff r a             -- * On fulfill
               -> Eff (TestControl ': r) a -- * The test, with @TestControl@
               -> Eff r a             -- * The test, without @TestControl@
runTestControl onFail onFulfill t = runError t >>= \testResult -> case testResult of
  Left (TestFailure s) -> onFail s
  Left Fulfilled -> onFulfill
  Right x -> return x

-- | Consider the test result OK.
fulfilled :: Member (Exc TestExitStatus) r => Eff r a
fulfilled = throwError Fulfilled

-- | Return the next event without handling it
peekEvent :: (forall a. f a -> Eff r l) -> Eff (Interact f r v ': r) (Either l v)
peekEvent f = interact (\a -> (\x -> (Defer, Left x)) <$> f a) (return . Right)

-- | Terminates with provided message, showing next event.
showNext :: (Show v, Show1Q f) => Eff (Interact f r v ': r) String
showNext = do
  p <- peekEvent (return . show1q)
  return $ case p of
    Left x -> "Next event: " ++ x
    Right v -> "Done with result: " ++ show v

-- | Terminates the test with error, showing provided reason and next event.
failure :: (Show v, Show1Q f) => String -- ^ Reason for test failure
  -> Eff (Interact f r v ': r) any
failure reason = do
  nextEvent <- showNext
  error $ "Test failed, reason: " ++ reason ++ "\n" ++ nextEvent

-- | Terminates test as a failure by showing the expectation and the event.
expecting :: (Show1Q f) => String -- ^ Noun phrase describing expectation
  -> f a -- ^ Unexpected event
  -> b
expecting expectation v = error ("Expecting " ++ expectation ++ ", but got " ++ show1q v)

unexpected :: (Show1Q f) => f a -> b
unexpected v = error ("Did not expect " ++ show1q v)

-- | Simple handler for Writer effects
runWriterIO :: (Monoid o, Member IO r)
  => IORef o
  -> Eff (Writer o ': r) a
  -> Eff r a
runWriterIO ioref = do
  handleRelay return $ \(Writer o) k -> do
    send (atomicModifyIORef ioref (\ref -> (ref `mappend` o, ())))
    k ()

-- | Simply @runM@ with its type restricted to @IO@
runIO :: Eff '[IO] a -> IO a
runIO = runM

-- | When an event occurs, provide a value @a@ to the test subject and a value @b@
-- to the test script.
expect :: forall f r v b.
  (forall a. f a -> Eff r (a, b)) -> Eff (Interact f r v ': r) b
expect f = E (inj1 (Interact (\x -> first Now <$> f x) (error "Unexpected program termination"))) (tsingleton return)

inj1 :: t v -> Union (t ': r) v
inj1 = inj

interact :: forall f r v b.
  (forall a. f a -> Eff r (Deferable a, b)) -> (v -> Eff r b) -> Eff (Interact f r v ': r) b
interact f f' = E (inj1 (Interact f f')) (tsingleton return)

-- | When an event occurs, provide a value to the test subject.
stub :: (forall b. f b -> Eff r b) -> Eff (Interact f r v ': r) ()
stub f = expect (fmap (\x -> (x,())) <$> f)

-- | Provide a value to the test subject, if and as long as matching
-- events occur. Returns the number of events that have been matched.
stubs :: (forall b. f b -> Eff r (Deferable b)) -> Eff (Interact f r v ': r) ()
stubs f = do
  join $ interact (
            \x -> do
              replyMaybe <- f x
              case replyMaybe of
                Now reply -> return (Now reply, stubs f)
                Defer -> return (Defer, (return ()))
            ) (const $ return (return ()))

-- | Provide a value to the test subject, if and as long as matching
-- events occur. Returns the number of events that have been matched.
collect :: (forall a. f a -> Eff r (Deferable (a, b))) -> Eff (Interact f r v ': r) [b]
collect f = do
  join $ interact (
            \x -> do
              replyMaybe <- f x
              case replyMaybe of
                Now (reply, spied) -> return (Now reply, (spied :) <$> collect f)
                Defer -> return (Defer, (return []))
            ) (const $ return (return []))

-- | Retrieve the result of the program. Fails if an effect of type @f@ is still pending.
result :: Show1Q f => Eff (Interact f r v ': r) v
result = interact unexpected return

result_ :: Eff (Interact f r v ': r) v
result_ = interact (error "Expected program result, not an effect.") return

defer :: Monad m => m (Deferable a)
defer = return Defer

now :: Monad m => a -> m (Deferable a)
now = return . Now

-- | Provide empty response to test subject, pass argument to test script
spy :: (Monad m, Monoid mm) => a -> m (mm, a)
spy a = return (mempty, a)

-- | Provide expectations for some effect.
-- @Eff '[Expect f]@ is a test script for @Eff '[f]@.
data Interact f r v a where
  Interact :: (forall b. f b -> Eff r (Deferable b, a)) -- ^ Maybe handle an effect
    -> (v -> Eff r a)                             -- ^ Handle the result
    -> Interact f r v a

-- | Zips together two programs: the subject and the test script.
--
-- The script gets to run side effects (the @more@ parameter) first
-- when there is such a choice.
--
-- The @more@ parameter determines what (computational or I/O) effects are
-- required for running the test. This makes it possible to write pure tests,
-- tests that explore all branches of nondeterministic choices, tests that
-- read from files dynamically, etc.
runInteractions
  :: forall a more b x.
     Eff (x ': more) a                  -- ^ Test subject
  -> Eff (Interact x more a ': more) b  -- ^ Test script
  -> Eff more b                         -- ^ Runnable test
runInteractions = loop where
  loop :: Eff (x ': more) a                  -- ^ Test subject
       -> Eff (Interact x more a ': more) b  -- ^ Test script
       -> Eff more b                         -- ^ Runnable test

  -- Done
  loop _subject _script@(Val b) = return b

  -- Side effects
  loop subject script@(E x qScript) = case decomp x of
    Left scriptSideEffect -> E  scriptSideEffect
                                (tsingleton (qComp qScript (loop subject)))
    Right (Interact onEvent onResult) -> case subject of
      Val subjectResult -> do
        c <- onResult subjectResult
        loop subject (qScript `qApp` c)
      E subjectEffect qSubject -> case decomp subjectEffect of
        Left sideEffect -> E sideEffect (tsingleton (qComp qSubject c))
           where c s = loop s script
        Right event -> do
          (maybeReply, spy) <- onEvent event
          case maybeReply of
            Now reply -> loop (qSubject `qApp` reply) (qScript `qApp` spy)
            Defer -> loop subject (qScript `qApp` spy)

-- | Like Show, but can be implemented for quantified values, like @forall a. Maybe a@. It is impossible to print the value inside the @Just@, but one can still distinguish between @Just@ and @Nothing@, and show that.
class Show1Q (f :: k -> *) where
  show1q :: f a -> String

instance Show1Q Maybe where
  show1q (Just _) = "Just"
  show1q Nothing = "Nothing"
