
instance Monad Maybe where
  return x = Just x

  (Just x) >>= k = k x
  Nothing >>= _  = Nothing

  
