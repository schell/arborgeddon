module Game.Animation where


class Animation a where
    tick :: Double -> a -> a
