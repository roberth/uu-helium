module DoublyLinkedTree where

import Utils (internalError)

data DoublyLinkedTree attributes = 
     DoublyLinkedTree { parent    :: Maybe (DoublyLinkedTree attributes)
                      , attribute :: attributes
                      , children  :: [DoublyLinkedTree attributes]
                      }
                      
root :: a -> [DoublyLinkedTree a] -> DoublyLinkedTree a
root a ts = DoublyLinkedTree Nothing a ts

node :: DoublyLinkedTree a -> a -> [DoublyLinkedTree a] -> DoublyLinkedTree a
node p a cs = DoublyLinkedTree (Just p) a cs                        

selectChild ::  Int -> DoublyLinkedTree a -> DoublyLinkedTree a
selectChild i tree = 
   case drop i (children tree) of
      []   -> internalError "TypeInferDoublyLinkedTreenceInfo.hs" "selectChild" "no such child"
      hd:_ -> hd 
      
selectRoot :: DoublyLinkedTree a -> DoublyLinkedTree a
selectRoot tree = maybe tree selectRoot (parent tree)
