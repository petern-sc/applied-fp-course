sequenceOption :: [Maybe a] -> Maybe [a]
sequenceOption [] = Just []
sequenceOption (Nothing : tail) = Nothing
sequenceOption (Just head : tail) = do
  tail' <- sequenceOption tail
  Just (head : tail')

sequenceOption2 :: [Maybe a] -> Maybe [a]
sequenceOption2 [] = Just []
sequenceOption2 (head : tail) = 
  let fa = fmap (:) head -- :: Maybe ([a] -> [a])
  in fa <*> sequenceOption2 tail

sequence2 :: Applicative m => [m a] -> m [a]
sequence2 [] = pure []
sequence2 (head : tail) = 
  let fa = fmap (:) head
  in fa <*> sequence2 tail
