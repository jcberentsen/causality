{-# LANGUAGE TemplateHaskell, OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module Examples.MontyHall where

import Model
import Evidence
import Likelyhood
import Population

import Data.String

-- http://en.wikipedia.org/wiki/Monty_Hall_problem

-- The car is the prize behind one of 3 doors!
-- Player chooses a door (we fix at 1, as it does not matter)
-- The Host REVEALS a losing door (not chosen and without car)
-- The player chooses to STAY or SWITCH to the remaining door
-- The content behind the chosen door is REVEALED

-- we fix the player choosing door 1 (The problem is the same if we choose another door)
player_chose_door_1 :: IsString name => Evidence name Bool
player_chose_door_1 = fact "user chose door 1"

win_door_1, win_door_2, win_door_3 :: IsString name => Evidence name Bool

win_door_1 = fact "car behind door 1"
win_door_2 = fact "car behind door 2"
win_door_3 = fact "car behind door 3"

host_reveals_2, host_reveals_3 :: IsString name => Evidence name Bool
host_reveals_2 = fact "host reveals empty 2"
host_reveals_3 = fact "host reveals empty 3"
-- host can't reveal door 1 as we fixed the player choice at door 1

player_wins, player_loses :: IsString name => Evidence name Bool
player_wins = fact "win"
player_loses = fact "lose"

-- likelyhood of car behind a door
-- only one door can have car

-- we need the population of car_behind_door_N
-- Alternative is a likelyhood of multiple facts
car_door_likelyhood :: IsString name => Alternatives name Bool
car_door_likelyhood = Alternatives [win_door_1, win_door_2, win_door_3]

host_opens :: IsString name => CausalModel name Bool
host_opens = Multiple
    [ Causally win_door_1 host_reveals_2 -- consider host can choose random here?
    , Causally win_door_2 host_reveals_3 -- forced
    , Causally win_door_3 host_reveals_2 -- forced
    ]

staying :: IsString name => CausalModel name Bool
staying = Multiple
    [ Causally win_door_1 player_wins
    , Causally win_door_2 player_loses
    , Causally win_door_3 player_loses
    ]

switching :: IsString name => CausalModel name Bool
switching = Multiple
    [ AllCause [win_door_1] player_loses
    , AllCause [win_door_2, host_reveals_3]  player_wins
    , AllCause [win_door_3, host_reveals_2]  player_wins
    ]

staying_game :: IsString name => CausalModel name Bool
staying_game = Multiple
    [ host_opens
    , staying
    ]

switching_game :: IsString name => CausalModel name Bool
switching_game = Multiple
    [ host_opens
    , switching
    ]

prize_potential :: IsString name => Potential name Float Bool
prize_potential = Alternatively car_door_likelyhood

win_count :: (IsString name, Ord name) => CausalModel name Bool -> Int
win_count game = population_count player_wins $ generate_population 3 prize_potential game
