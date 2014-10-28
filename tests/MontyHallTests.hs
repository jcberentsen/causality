{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module MontyHallTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Examples.MontyHall

import Model
import Evidence
import Likelyhood
import Population
import Observations

monty_hall_test_group = $(testGroupGenerator)

case_car_toss = select (0.1::Double) (car_door_likelyhood :: Alternatives String Bool) @?= [win_door_1, void win_door_2, void win_door_3]

case_car_population = conclusions @?=
        [ conclude [win_door_1, void win_door_2, void win_door_3]
        , conclude [void win_door_1, win_door_2, void win_door_3]
        , conclude [void win_door_1, void win_door_2, win_door_3]
        ]
    where
        conclusions = generate_population 3 potential (Ignorance::CausalModel String Bool)
        potential = Alternatively car_door_likelyhood :: Potential String Float Bool

prop_staying_game_win_1 = has_fact player_wins $ eval_causalmodel [win_door] staying_game
    where win_door :: Evidence String Bool
          win_door = win_door_1

prop_staying_game_lose_2 = has_fact (player_loses :: Evidence String Bool) $ eval_causalmodel [win_door_2] staying_game
prop_staying_game_lose_3 = has_fact (player_loses :: Evidence String Bool) $ eval_causalmodel [win_door_3] staying_game

prop_switching_game_lose_1 = has_fact (player_loses :: Evidence String Bool) $ eval_causalmodel [win_door_1] switching_game
prop_switching_game_win_2 = has_fact (player_wins :: Evidence String Bool) $ eval_causalmodel [win_door_2] switching_game
prop_switching_game_win_3 = has_fact (player_wins :: Evidence String Bool) $ eval_causalmodel [win_door_3] switching_game

case_switching_population = win_count (switching_game :: CausalModel String Bool) @?= 2
case_staying_population = win_count (staying_game :: CausalModel String Bool) @?= 1
prop_MontyHall_switching_beats_staying = win_count (switching_game :: CausalModel String Bool) > win_count (staying_game :: CausalModel String Bool)
