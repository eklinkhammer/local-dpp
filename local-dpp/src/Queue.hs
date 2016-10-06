module Queue
  (
    Queue,
    push,
    pop,
    count,
    create
  ) where

data Queue a = Queue [a] [a]

instance (Show a) => Show (Queue a) where
  show (Queue back front) = show $ front ++ (reverse back)
  
create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push elem (Queue back front) = Queue (elem:back) front

pop :: Queue a -> (Maybe a, Queue a)
pop q@(Queue []   [])    = (Nothing, q)
pop   (Queue back [])    = pop (Queue [] (reverse back))
pop   (Queue back front) = (Just $ head front, Queue back (tail front))

count :: Queue a -> Int
count (Queue front back) = length front + length back

