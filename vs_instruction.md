# Tetris Vs

## Requirements:
- Swi Prolog
- A local web server (for example [Simple Web Server](https://simplewebserver.org/)), or alternatively, you can use VSCode with the Live Server extension installed ([How to enable Live Server on Visual Studio Code](https://www.geeksforgeeks.org/how-to-enable-live-server-on-visual-studio-code/)).
- The whole project has been developed testing it using Live Server.

## Installation:

### Prolog server:
- Run `tetrisws.pl` with Swi Prolog, this will start the backend server.

### HTML/JS server:
- Load the `VSTETRIS` folder in VSCode.
- Run `tetris.html` with VSCode using "Open with Live Server".
  OR
- Load the `VSTETRIS` folder in Simple Web Server.
- Choose a port number (e.g., 8089).
- Run the game using the following hyperlink: [http://127.0.0.1:8089/tetris.html](http://127.0.0.1:8089/tetris.html)

## How to play:

### Player 1:
- WASD keys move the Player 1 tetromino.
- Spacebar rotates the Player 1 tetromino.

### Player 2:
- Arrow keys move the Player 2 tetromino.
- Right CTRL rotates the Player 2 tetromino.

- Press 'P' to put the game in pause mode (for both players).

## How to use the explanation mode (for Firefox browser):
- If the explanation mode is turned on, the game will be paused at each AI move.
- In order to read the explanation, just right-click on the game and select "Inspect," then "Console"; now you can read the explanation of each move made by the AI agent.
- Press 'P' to unpause the game.

### For other browsers:
- [Google Chrome DevTools Console Log](https://developer.chrome.com/docs/devtools/console/log)
- [Microsoft Edge DevTools Console Log](https://learn.microsoft.com/en-us/microsoft-edge/devtools-guide-chromium/console/console-log)
