# tic-tac-toe-backend

## Install

You'll need haskell stack to run this code.  You can find instructions to install 
this at [https://docs.haskellstack.org/en/stable/README/]. 

Once you install you can move into the `tic-tac-toe-backend` folder and run 
`stack install` to install all dependencies.

## Running
There are two executable modes `stack run -- migrate-all` and 
`stack run -- run-server`.  You need to run `migrate-all` to create and setup the 
database, once you do you can run `run-server` and that will start the server on 
port `5000` for use.  

## Routes
Running the server will start the following REST endpoints:

- Users
    + `GET /1/users`
        * Returns a JSON list of all users in the format:
        ```
        {
            "id": Int - db user id,
            "userName": String - The name of the returned user,
            "userEmail": String - The email address of the returned user,
            "userPassword": String - The user's password // TODO: very bad lol gotta get rid of this
        }
        ```
    + `GET /1/users/:id`
        * Gets the user based on the provided id
        * Returns user in the previous format (without the id)
        * Returns 404 on user id not existing
    + `POST /1/users`
        * Inserts the user based on the provided data.
        * Body provided is in JSON format with the previous configuration (without the id)
        * Returns 201 with a JSON object containing the id of the created user
    + `DELETE /1/users/:id`
        * Deletes the user with the provided id
        * Returns 200 with the value "Success!"
        
- Players
    + `GET /1/players`
        * Returns a JSON list of all players in the format:
        ```
        {
            "id": Int - db player id,
            "playerUserId": Int - User id this player is associated with
            "playerGameId": Int - Game id this player is playing
            "playerCode": String - The piece the player is playing with eg "X" or "O"
            "playerOrder": Int - the order the players in the game. 
        }
        ```
    + `GET /1/players/:id`
        * Gets the player based on the provided id
        * Returns player in the previous format (without the id)
        * Returns 404 on player id not existing
    + `POST /1/players`
        * Inserts the player based on the provided data.
        * Body provided is in JSON format with the previous configuration (without the id)
        * Returns 201 with a JSON object containing the id of the created player
    + `DELETE /1/players/:id`
        * Deletes the player with the provided id
        * Returns 200 with the value "Success!"
        
- Games
    + `GET /1/games`
        * Returns a JSON list of all games in the format:
        ```
        {
            "id": Int - db game id,
            "gameCode": String - the game code you can use to reference the game
            "gameSize": String - The size of the game formatted "Int,Int" (zero indexed)
        }
        ```
    + `GET /1/games/:id`
        * Gets the game based on the provided id
        * Returns game in the previous format (without the id)
        * Returns 404 on game id not existing
    + `POST /1/games`
        * Inserts the game based on the provided data.
        * Body provided is in JSON format with the previous configuration (without the id)
        * Returns 201 with a JSON object containing the id of the created game
    + `DELETE /1/games/:id`
        * Deletes the game with the provided id
        * Returns 200 with the value "Success!"

- Moves
    + `GET /1/moves`
        * Returns a JSON list of all moves in the format:
        ```
        {
            "id": Int - db move id,
            "movePlayerId": Int - Player id this move is associated with
            "moveGameId": Int - Game id this move is part of
            "movePos": String - The position of the move in format "Int,Int" (zero indexed)
        }
        ```
    + `GET /1/moves/:id`
        * Gets the move based on the provided id
        * Returns move in the previous format (without the id)
        * Returns 404 on move id not existing
    + `POST /1/moves`
        * Inserts the move based on the provided data.
        * Body provided is in JSON format with the previous configuration (without the id)
        * Returns 201 with a JSON object containing the id of the created move
    + `DELETE /1/moves/:id`
        * Deletes the move with the provided id
        * Returns 200 with the value "Success!"