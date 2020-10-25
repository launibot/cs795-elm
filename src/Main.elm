module Main exposing (..)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy as Lazy
import Html exposing (Html)
import Html.Attributes exposing (href, rel, style)
import Json.Decode as Decode
import Random
import Time


type alias Position =
    { x : Int
    , y : Int
    }


type alias Snake =
    List Position


type alias Food =
    List Position


type TileKind
    = TileEmpty
    | TileBody
    | TileFood


type GameState
    = Playing
    | Lost String
    | NotStarted
    | Paused


type alias Direction =
    ( Int, Int )


type alias Directions =
    { up : Direction
    , down : Direction
    , left : Direction
    , right : Direction
    , none : Direction
    }


type alias Config =
    { fieldWidth : Int
    , fieldHeight : Int
    , tileSize : Int
    , foodCount : Int
    , initialSnake : List Position
    , initialUpdateRate : Float
    , initialDirection : Direction
    }


config : Config
config =
    { fieldWidth = 30
    , fieldHeight = 30
    , tileSize = 15
    , foodCount = 5
    , initialSnake = [ Position 12 15, Position 11 15, Position 10 15 ]
    , initialUpdateRate = 200
    , initialDirection = directions.right
    }


directions : Directions
directions =
    { up = ( 0, -1 )
    , down = ( 0, 1 )
    , left = ( -1, 0 )
    , right = ( 1, 0 )
    , none = ( 0, 0 )
    }


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias PlayField =
    { width : Int
    , height : Int
    , data : List TileKind
    }


type alias Model =
    { snake : Snake
    , food : Food
    , field : PlayField
    , direction : Direction
    , directionBuffer : List Direction
    , gameState : GameState
    , updateRate : Float
    , score : Int
    }


type CollisionTestResult
    = NoCollision
    | AteFood
    | BitSelf
    | HitWall


type Msg
    = Move Direction
    | Tick Time.Posix
    | RandomPositions (List Position)
    | IgnoreKey
    | ChangeUpdateRate
    | TogglePause
    | Start


newPlayField : Int -> Int -> PlayField
newPlayField width height =
    PlayField width height (List.repeat (width * height) TileEmpty)


newModel : GameState -> Model
newModel gameState =
    { snake = config.initialSnake
    , food = []
    , field = newPlayField config.fieldWidth config.fieldHeight
    , direction = config.initialDirection
    , directionBuffer = [ config.initialDirection ]
    , gameState = gameState
    , updateRate = config.initialUpdateRate
    , score = 0
    }


randomPositions : Int -> Cmd Msg
randomPositions count =
    Random.generate RandomPositions <|
        Random.list count positionGenerator


positionGenerator : Random.Generator Position
positionGenerator =
    Random.map2
        (\x y -> Position x y)
        (Random.int 0 <| config.fieldWidth - 1)
        (Random.int 0 <| config.fieldHeight - 1)


init : () -> ( Model, Cmd Msg )
init _ =
    ( newModel NotStarted
    , randomPositions config.foodCount
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomPositions positions ->
            ( { model
                | food = model.food ++ diffList positions model.snake
              }
            , Cmd.none
            )

        Move newDirection ->
            if model.gameState == Playing && allowDirection newDirection model.directionBuffer then
                ( { model
                    | directionBuffer = model.directionBuffer ++ [ newDirection ]
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Tick posixTime ->
            let
                newBuffer =
                    updateDirectionBuffer model.directionBuffer

                ( newSnake, removedPart ) =
                    updateSnake newBuffer model.snake

                ( collisionTestResult, snakeHead ) =
                    evalCollision newSnake model.food

                ( newFood, newScore ) =
                    case snakeHead of
                        Just h ->
                            if collisionTestResult == AteFood then
                                ( List.filter (\x -> x /= h) model.food
                                , model.score + 1
                                )

                            else
                                ( model.food, model.score )

                        Nothing ->
                            ( model.food, model.score )
            in
            if model.gameState == Playing then
                ( { model
                    | snake =
                        case collisionTestResult of
                            AteFood ->
                                newSnake ++ removedPart

                            HitWall ->
                                model.snake

                            _ ->
                                newSnake
                    , gameState = collisionToGameStatus collisionTestResult
                    , food = newFood
                    , score = newScore
                    , updateRate =
                        if model.updateRate > 50 && collisionTestResult == AteFood then
                            model.updateRate - 5

                        else
                            model.updateRate
                    , directionBuffer = newBuffer
                  }
                , if List.length model.food < 2 then
                    randomPositions 5

                  else
                    Cmd.none
                )

            else
                ( model, Cmd.none )

        ChangeUpdateRate ->
            ( { model
                | updateRate = model.updateRate - 20
              }
            , Cmd.none
            )

        TogglePause ->
            if model.gameState == Paused then
                ( { model | gameState = Playing }, Cmd.none )

            else if model.gameState == Playing then
                ( { model | gameState = Paused }, Cmd.none )

            else
                ( model, Cmd.none )

        Start ->
            if model.gameState == NotStarted then
                ( { model | gameState = Playing }, Cmd.none )

            else if isLost model.gameState then
                ( newModel Playing, randomPositions config.foodCount )

            else
                ( model, Cmd.none )

        IgnoreKey ->
            ( model, Cmd.none )


allowDirection : Direction -> List Direction -> Bool
allowDirection ( x, y ) directionBuffer =
    let
        lastDirection =
            List.head <| List.reverse directionBuffer
    in
    case lastDirection of
        Just ( lastX, lastY ) ->
            not (x + lastX == 0 || y + lastY == 0)
                && (( lastX, lastY ) /= ( x, y ))

        _ ->
            True


updateDirectionBuffer : List Direction -> List Direction
updateDirectionBuffer directionBuffer =
    case directionBuffer of
        [] ->
            directionBuffer

        [ _ ] ->
            directionBuffer

        h :: rest ->
            rest


diffList : List a -> List a -> List a
diffList aList bList =
    List.filter (\c -> not <| List.member c bList) aList


updateSnake : List Direction -> Snake -> ( Snake, List Position )
updateSnake directionBuffer snake =
    let
        ( xDir, yDir ) =
            Maybe.withDefault directions.none <| List.head directionBuffer

        h =
            Maybe.withDefault (Position -1 -1) <| List.head snake

        newHead =
            Position (h.x + xDir) (h.y + yDir)

        newBody =
            List.take (List.length snake - 1) snake

        removedPart =
            List.drop (List.length snake - 1) snake
    in
    ( [ newHead ] ++ newBody, removedPart )


evalCollision : Snake -> Food -> ( CollisionTestResult, Maybe Position )
evalCollision snake food =
    case snake of
        h :: t ->
            if List.member h t then
                ( BitSelf, Just h )

            else if h.x < 0 || h.y < 0 || h.x == config.fieldWidth || h.y == config.fieldHeight then
                ( HitWall, Just h )

            else if List.member h food then
                ( AteFood, Just h )

            else
                ( NoCollision, Nothing )

        _ ->
            ( NoCollision, Nothing )


collisionToGameStatus : CollisionTestResult -> GameState
collisionToGameStatus collisionTestResult =
    case collisionTestResult of
        NoCollision ->
            Playing

        AteFood ->
            Playing

        BitSelf ->
            Lost "YIKES! YOU BIT YOURSELF!"

        HitWall ->
            Lost "OOF THAT'S THE WALL!"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every model.updateRate Tick
        , Browser.Events.onKeyDown keyDecoder
        ]


isLost : GameState -> Bool
isLost gameState =
    case gameState of
        Lost _ ->
            True

        _ ->
            False


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map keyToMessage (Decode.field "key" Decode.string)


keyToMessage : String -> Msg
keyToMessage string =
    case String.uncons string of
        Just ( char, "" ) ->
            case String.toLower <| String.fromChar char of
                "q" ->
                    ChangeUpdateRate

                "p" ->
                    TogglePause

                " " ->
                    Start

                _ ->
                    IgnoreKey

        _ ->
            case string of
                "ArrowUp" ->
                    Move directions.up

                "ArrowDown" ->
                    Move directions.down

                "ArrowLeft" ->
                    Move directions.left

                "ArrowRight" ->
                    Move directions.right

                _ ->
                    IgnoreKey


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "IT'S ... SNAKE!"
    , body =
        [ Element.layout
            [ Font.family
                [ Font.external
                    { url = "https://fonts.googleapis.com/css?family=Russo+One"
                    , name = "Russo One"
                    }
                , Font.sansSerif
                ]
            , Font.color gameColors.body
            , Background.color gameColors.frame1
            ]
          <|
            el
                ([ centerX ]
                    ++ styleGameFrame gameColors.frame1 gameColors.frame1 False
                )
                (if model.gameState == NotStarted then
                    viewStart

                 else
                    Lazy.lazy viewGame model
                )
        ]
    }


viewStart : Element Msg
viewStart =
    el (styleGameFrame gameColors.frame2 gameColors.black True) <|
        column [ centerX, centerY, spacing 40, width <| px 300 ]
            [ viewTitle
            , column [ spacing 15 ]
                [ paragraph [ Font.center ]
                    [ text "USE "
                    , el [ Font.color gameColors.yellow ] (text "ARROW KEYS")
                    , text " TO MOVE YOUR SNAKE"
                    ]
                , paragraph [ Font.center ]
                    [ text "PRESS "
                    , el [ Font.color gameColors.yellow ] (text "P")
                    , text " TO PAUSE THE GAME"
                    ]
                , paragraph [ Font.center ] [ text "YOU WILL DIE IF YOUR BITE YOURSELF OR HIT THE WALL." ]
                , paragraph [ Font.center ] [ text "THE MORE YOU EAT, THE LONGER YOU GET AND THE FASTER YOU MOVE." ]
                ]
            , paragraph [ Font.center, Font.color gameColors.body ]
                [ text "PRESS ", el [ Font.color gameColors.yellow ] (text "SPACEBAR"), text " TO START" ]
            ]


gameColors =
    { black = rgb255 0 0 0
    , yellow = rgb255 255 255 0
    , body = rgb255 255 255 255
    , snake = rgb255 0 220 0
    , tile = rgb255 50 20 20
    , food = rgb255 180 0 0
    , wall = rgb255 80 50 50
    , frame1 = rgb255 60 110 60
    , frame2 = rgb255 80 180 80
    , title = rgb255 0 220 0
    }


viewGame : Model -> Element Msg
viewGame model =
    el (styleGameFrame gameColors.frame2 gameColors.black True) <|
        column
            ([ centerX, centerY, spacing 30 ]
                ++ viewMessage model.gameState
            )
        <|
            [ viewTitle
            , el [] <| viewField model
            , viewScore model.score
            ]


viewField : Model -> Element Msg
viewField model =
    let
        width_ =
            model.field.width * config.tileSize

        tileCount =
            List.length model.field.data
    in
    el
        [ Border.color gameColors.wall
        , Border.width 15
        , Border.rounded 8
        ]
        (el (viewActionField model) <|
            Lazy.lazy3 viewStaticField TileEmpty tileCount width_
        )


viewStaticField : TileKind -> Int -> Int -> Element Msg
viewStaticField tileKind count width_ =
    wrappedRow
        [ width <| px width_ ]
    <|
        List.map (\_ -> viewTile tileKind Nothing) (List.repeat count 0)


viewActionField : Model -> List (Element.Attribute Msg)
viewActionField model =
    let
        element =
            \a tileKind -> Element.inFront <| viewTile tileKind <| Just a
    in
    List.map (\x -> element x TileBody) model.snake
        ++ List.map (\x -> element x TileFood) model.food


viewTitle : Element Msg
viewTitle =
    el
        [ Font.color gameColors.title
        , Font.size 50
        , Font.bold
        , centerX
        ]
        (text "SNAKE!")


viewScore : Int -> Element Msg
viewScore score =
    row [ centerX ]
        [ el [ width <| px 100 ] (text "SCORE : ")
        , el [ width <| px 20 ] (text <| String.fromInt score)
        ]


viewMessage : GameState -> List (Element.Attribute Msg)
viewMessage status =
    case status of
        Lost string ->
            [ Element.inFront
                (el
                    [ centerX
                    , centerY
                    , padding 20
                    , Border.solid
                    , Border.color gameColors.body
                    , Border.width 1
                    , Border.rounded 5
                    , Border.color gameColors.black
                    ]
                 <|
                    column
                        [ spacing 10 ]
                        [ paragraph [ Font.center ] [ text string ]
                        , paragraph [ Font.center ] [ text "PRESS SPACEBAR TO RESTART THE GAME" ]
                        ]
                )
            ]

        _ ->
            []


viewTile : TileKind -> Maybe Position -> Element Msg
viewTile tileKind position =
    el
        (styleTile tileKind
            ++ (case position of
                    Just p ->
                        [ moveRight <| toFloat (p.x * config.tileSize)
                        , moveDown <| toFloat (p.y * config.tileSize)
                        ]

                    Nothing ->
                        []
               )
        )
        none


styleTile : TileKind -> List (Element.Attribute Msg)
styleTile tileContent =
    let
        tileColor =
            case tileContent of
                TileEmpty ->
                    gameColors.tile

                TileBody ->
                    gameColors.snake

                TileFood ->
                    gameColors.food
    in
    [ width <| px config.tileSize
    , height <| px config.tileSize
    , Background.color tileColor
    , Border.color gameColors.black
    , Border.width 1
    , Border.solid
    , Border.rounded 3
    ]


styleGameFrame : Element.Color -> Element.Color -> Bool -> List (Element.Attribute Msg)
styleGameFrame brColor bgColor rounded_ =
    [ width (Element.fill |> Element.maximum 700 |> Element.minimum 500)
    , height fill
    , Background.color bgColor
    , Border.color brColor
    , Border.width 10
    ]
        ++ (if rounded_ then
                [ Border.rounded 10 ]

            else
                []
           )
