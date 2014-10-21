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

host_reveals_2, host_reveals_3 :: Evidence String Bool
host_reveals_2 = fact "host reveals empty 2"
host_reveals_3 = fact "host reveals empty 3"
-- host can't reveal door 1 as we fixed the player choice at door 1

player_wins, player_loses :: Evidence String Bool
player_wins = fact "win"
player_loses = fact "lose"

-- likelyhood of car behind a door
-- only one door can have car

-- we need the population of car_behind_door_N
-- Alternative is a likelyhood of multiple facts
car_door_likelyhood :: Alternatives String Bool
car_door_likelyhood = Alternatives [win_door_1, win_door_2, win_door_3]

case_car_toss = select (0.1::Double) car_door_likelyhood @?= [win_door_1, void win_door_2, void win_door_3]

case_car_population = conclusions @?=
        [ conclude [win_door_1, void win_door_2, void win_door_3]
        , conclude [void win_door_1, win_door_2, void win_door_3]
        , conclude [void win_door_1, void win_door_2, win_door_3]
        ]
    where
        conclusions = generate_population 3 potential  (Ignorance::CausalModel String Bool)
        potential = Alternatively car_door_likelyhood :: Potential String Float Bool

host_opens = Multiple
    [ Causally win_door_1 host_reveals_2 -- consider host can choose random here?
    , Causally win_door_2 host_reveals_3 -- forced
    , Causally win_door_3 host_reveals_2 -- forced
    ]

staying = Multiple
    [ Causally win_door_1 player_wins
    , Causally win_door_2 player_loses
    , Causally win_door_3 player_loses
    ]

switching = Multiple
    [ AllCause [win_door_1] player_loses
    , AllCause [win_door_2, host_reveals_3]  player_wins
    , AllCause [win_door_3, host_reveals_2]  player_wins
    ]

staying_game = Multiple
    [ host_opens
    , staying
    ]

switching_game = Multiple
    [ host_opens
    , switching
    ]

prop_staying_game_win_1 = has_fact player_wins $ eval_causalmodel [win_door_1] staying_game
prop_staying_game_lose_2 = has_fact player_loses $ eval_causalmodel [win_door_2] staying_game
prop_staying_game_lose_3 = has_fact player_loses $ eval_causalmodel [win_door_3] staying_game

prop_switching_game_lose_1 = has_fact player_loses $ eval_causalmodel [win_door_1] switching_game
prop_switching_game_win_2 = has_fact player_wins $ eval_causalmodel [win_door_2] switching_game
prop_switching_game_win_3 = has_fact player_wins $ eval_causalmodel [win_door_3] switching_game

prize_potential = Alternatively car_door_likelyhood :: Potential String Float Bool
win_count game = population_count player_wins $ generate_population 3 prize_potential game

case_switching_population = win_count switching_game @?= 2
case_staying_population = win_count staying_game @?= 1
prop_MontyHall_switching_beats_staying = win_count switching_game > win_count staying_game
