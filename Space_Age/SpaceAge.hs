module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury | Venus | Earth | Mars | Jupiter | Saturn | Uranus | Neptune

-- Number of seconds in one Earth year
earthYearInSeconds :: Float
earthYearInSeconds = 31557600

-- Orbital periods of planets in Earth years
orbitalPeriod :: Planet -> Float
orbitalPeriod planet = case planet of
    Mercury -> 0.2408467
    Venus   -> 0.61519726
    Earth   -> 1.0
    Mars    -> 1.8808158
    Jupiter -> 11.862615
    Saturn  -> 29.447498
    Uranus  -> 84.016846
    Neptune -> 164.79132

-- Calculate age on a given planet
ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (earthYearInSeconds * orbitalPeriod planet)
