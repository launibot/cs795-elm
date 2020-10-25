module SnakeTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


inputTests =
    describe "Snake Input handling"
        [ test "Change update rate on q" <|
            \_ ->
                keyToMessage "q"
                    |> Expect.equal ChangeUpdateRate
        , test "Pause game on p" <|
            \_ ->
                keyToMessage "p"
                    |> Expect.equal TogglePause
        , test "Space to start" <|
            \_ ->
                keyToMessage " "
                    |> Expect.equal Start
        , test "ArrowUp to move up" <|
            \_ ->
                keyToMessage "ArrowUp"
                    |> Expect.equal (Move directions.up)
        , test "ArrowDown to move down" <|
            \_ ->
                keyToMessage "ArrowDown"
                    |> Expect.equal (Move directions.down)
        , test "ArrowLeft to move left" <|
            \_ ->
                keyToMessage "ArrowLeft"
                    |> Expect.equal (Move directions.left)
        , test "ArrowRight to move right" <|
            \_ ->
                keyToMessage "ArrowRight"
                    |> Expect.equal (Move directions.right)
        ]


collisionTests =
    describe "Collision event test"
        [ test "Continue playing without collision" <|
            \_ ->
                collisionToGameStatus NoCollision
                    |> Expect.equal Playing
        , test "Continue playing after eating" <|
            \_ ->
                collisionToGameStatus AteFood
                    |> Expect.equal Playing
        , test "Biting yourself is a lose" <|
            \_ ->
                collisionToGameStatus BitSelf
                    |> Expect.equal (Lost "YIKES! YOU BIT YOURSELF!")
        , test "Hitting the wall is a lose" <|
            \_ ->
                collisionToGameStatus HitWall
                    |> Expect.equal (Lost "OOF THAT'S THE WALL!")
        ]


gameStateLossTests =
    describe "Verify states map to a loss"
        [ test "Playing is not a loss" <|
            \_ ->
                isLost Playing
                    |> Expect.equal False
        , test " Lost is a loss" <|
            \_ ->
                isLost (Lost "Anything honestly")
                    |> Expect.equal True
        ]
