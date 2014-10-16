{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module MontyHallTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Model
import Evidence
import Likelyhood
import Population
import Observations

monty_hall_test_group = $(testGroupGenerator)

-- http://en.wikipedia.org/wiki/Monty_Hall_problem

-- The car is the prize behind one of 3 doors!
-- Player chooses a door (we fix at 1, as it does not matter)
-- The Host REVEALS a losing door (not chosen and without car)
-- The player chooses to STAY or SWITCH to the remaining door
-- The content behind the chosen door is REVEALED

-- we fix the player choosing door 1 (The problem is the same if we choose another door)
player_chose_door_1 :: Evidence String Bool
player_chose_door_1 = fact "user chose door 1"

win_door_1, win_door_2, win_door_3 :: Evidence String Bool

win_door_1 = fact "car behind door 1"
win_door_2 = fact "car behind door 2"
win_door_3 = fact "car behind door 3"

host_reveals_door_2, host_reveals_door_3 :: Evidence String Bool
host_reveals_door_2 = fact "host opened 2"
host_reveals_door_3 = fact "host opened 3"
-- host can't reveal door 1 as we fixed the player choice at door 1

-- likelyhood of car behind a door
-- only one door can have car

-- we need the population of car_behind_door_N
-- Alternative is a likelyhood of multiple facts
car_door_likelyhood :: Alternatives String Bool
car_door_likelyhood = Alternatives [win_door_1, win_door_2, win_door_3]

case_car_toss = select 0.1 car_door_likelyhood @?= [win_door_1, void win_door_2, void win_door_3]

case_car_population = conclusions @?=
        [ conclude [win_door_1, void win_door_2, void win_door_3]
        , conclude [void win_door_1, win_door_2, void win_door_3]
        , conclude [void win_door_1, void win_door_2, win_door_3]
        ]
    where
        conclusions = generate_population 3 (Alternatively car_door_likelyhood) (Ignorance::CausalModel String Bool)

-- -- prop_fail = False

