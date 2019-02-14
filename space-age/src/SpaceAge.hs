module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = case planet of
    Earth -> seconds / (normalize 1)
    Venus -> seconds / (normalize 0.61519726)
    Mercury -> seconds / (normalize 0.2408467)
    Mars -> seconds / (normalize 1.8808158)
    Jupiter -> seconds / (normalize 11.862615)
    Saturn -> seconds / (normalize 29.447498)
    Uranus -> seconds / (normalize 84.016846)
    Neptune -> seconds / (normalize 164.79132)
    where
        normalize modifier = modifier*31557600
