import Tetromino from "./Tetromino.js";
import MapperAI from "./MapperAI.js";
export default class GameBoard {
    gameBoardW = 10;
    gameBoardH = 20;

    #startX = 4;
    #startY = 0;

    #tetraX = 0;
    #tetraY = 0;

    //true: p1
    //false: p2
    turn = false;

    numberOfLevels = 5;

    level = 1;

    scoreP1 = 0;
    scoreP2 = 0;

    #p1NextTetromino = null;
    #p2NextTetromino = null;

    stopPlaying = false;
    gameOver = false;
    p1Wins = false;
    p2Wins = false;
    draw = false;

    #tetrominoMoving = false;

    tetromino = null;

    fetched = false;

    #interval = null;
    #intervalMs = 1000;

    #levelInterval = null;

    sec = 0;

    gameBoardMatrix = [...Array(this.gameBoardH)].map(e => Array(this.gameBoardW).fill(0));

    #p1Rotate = false;
    #p1Left = false;
    #p1Right = false;
    #p1Down = false;

    #p2Rotate = false;
    #p2Left = false;
    #p2Right = false;
    #p2Down = false;

    #pressPause = false;

    pause = false;

    #aiMapper = null;
    #aiEnabled = false;
    #readMoves = false;
    aiMoves = [];
    tetraminoPositionsMatrix = [];

    //all the keyCode here: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode
    #keydownP1 = event => {
        //P1 keys
        if (event.code == "Space") {
            this.#p1Rotate = true;
        }
        if (event.code == "KeyA") {
            this.#p1Left = true;
        }
        if (event.code == "KeyD") {
            this.#p1Right = true;
        }
        if (event.code == "KeyS") {
            this.#p1Down = true;
        }
    }

    #keydownP2 = event => {
        //P2 keys
        if (event.code == "ControlRight") {
            this.#p2Rotate = true;
        }
        if (event.code == "ArrowLeft") {
            this.#p2Left = true;
        }
        if (event.code == "ArrowRight") {
            this.#p2Right = true;
        }
        if (event.code == "ArrowDown") {
            this.#p2Down = true;
        }
    }

    #keydownPause = event => {
        if (event.code == "KeyP") {
            this.#pressPause = true;
        }
    }

    constructor(option) {
        var mov = this;
        this.playersNumber = option[0];
        this.playerOne = option[1];
        this.playerTwo = option[2];
        this.#aiMapper = new MapperAI();

        //assign the proper event listener for human player
        if (this.playerOne == 'Player') {
            document.addEventListener("keydown", this.#keydownP1);
        } else if (this.playerOne == 'AI') {
            this.#aiEnabled = true;
        }

        if (this.playerTwo == 'Player') {
            document.addEventListener("keydown", this.#keydownP2);
        } else if (this.playerTwo == 'AI') {
            this.#aiEnabled = true;
        }

        if (this.#aiEnabled && !this.gameOver) {
            this.#aiMapper.reset();
        }

        document.addEventListener("keydown", this.#keydownPause);

        this.#p1NextTetromino = new Tetromino();
        this.#p2NextTetromino = new Tetromino();
        this.p1NextTetrominoIndex = this.#p1NextTetromino.color;
        this.p2NextTetrominoIndex = this.#p2NextTetromino.color;
        this.#setIntervalMovement(this.#intervalMs);
        this.#levelInterval = setInterval(function () { if (!mov.pause) { mov.#nextSec() } }, 1000);
    }

    gameLoop() {
        if (!this.stopPlaying) {
            if (!this.pause) {
                if (!this.#tetrominoMoving) {
                    this.#checkCompleteLine();
                    this.#tetraX = this.#startX;
                    this.#tetraY = this.#startY;
                    this.#tetrominoMoving = true;

                    //switch turn when in VS mode, avoid it elsewhere
                    if (this.playersNumber == 2) {
                        this.turn = !this.turn;
                    } else {
                        this.turn = true;
                    }
                    this.#assignNextTetromino(this.turn);
                    this.gameOver = !(this.#checkCollision());
                    //this.pause = true;
                    if (this.#aiEnabled && !this.gameOver) {
                        this.#aiMapper.getSolution(this);
                        
                    }
                } else {
                    //then allows for player movement
                    if (this.#aiEnabled) {
                        this.#readMoves = true;
                        this.#executeAiMoves();
                    }
                    this.#move(false)
                }
            }
            this.#setPause();
        } else {
            this.#placeTetramino();
            clearInterval(this.#interval);
            clearInterval(this.#levelInterval);
        }

        this.stopPlaying = (this.gameOver || this.p1Wins || this.p2Wins || this.draw);
    }

    #setIntervalMovement(intervalMs) {
        var mov = this;
        mov.#intervalMs = intervalMs;
        this.#interval = setInterval(function () { if (!mov.pause && !mov.#aiEnabled) { mov.#move(true) } }, mov.#intervalMs);
    }

    #nextSec() {
        this.sec = (this.sec + 1) % 60;
        if (this.sec == 0) {
            clearInterval(this.#interval);
            this.#setIntervalMovement(this.#intervalMs * 0.8);
            this.level = this.level + 1;
        }

        //in solo mode the game goes on forever
        if (this.playersNumber == 2) {
            if (this.level > this.numberOfLevels) {
                if (this.scoreP1 > this.p2Wins) {
                    this.p1Wins = true;
                } else if (this.scoreP1 < this.p2Wins) {
                    this.p2Wins = true;
                } else if (this.scoreP1 == this.p2Wins) {
                    this.draw = true;
                }
            }
        }
    }

    #assignNextTetromino(turn) {
        if (turn) {
            this.tetromino = Object.assign(this.#p1NextTetromino);
            this.#p1NextTetromino = new Tetromino();
            this.p1NextTetrominoIndex = this.#p1NextTetromino.color;
        } else {
            this.tetromino = Object.assign(this.#p2NextTetromino);
            this.#p2NextTetromino = new Tetromino();
            this.p2NextTetrominoIndex = this.#p2NextTetromino.color;
        }
    }

    #rotate() {
        this.tetromino.rotate();
    }

    #undoRotation() {
        this.tetromino.undoRotation();
    }

    #left() {
        this.#tetraX = this.#tetraX - 1;
    }

    #right() {
        this.#tetraX = this.#tetraX + 1;
    }

    #down() {
        this.#tetraY = this.#tetraY + 1;
        //check if down movement is legit
        //non legit down movement blocks the tetramino on the board
        if (!this.#checkCollision()) {
            this.#tetrominoMoving = false;
            this.#undoDown();
        }
    }

    #undoDown() {
        this.#tetraY = this.#tetraY - 1;
    }

    #move(forceDown) {
        if ((this.#p1Rotate && this.turn) || (this.#p2Rotate && !this.turn)) {
            this.#rotate();
            //check if rotation is legit
            if (!this.#checkCollision()) {
                this.#undoRotation();
            }

        }
        if ((this.#p1Left && this.turn) || (this.#p2Left && !this.turn)) {
            this.#left();
            //check if left movement is legit
            if (!this.#checkCollision()) {
                this.#right();
            }
        }
        if ((this.#p1Right && this.turn) || (this.#p2Right && !this.turn)) {
            this.#right();
            //check if right movement is legit
            if (!this.#checkCollision()) {
                this.#left();
            }
        }
        if ((this.#p1Down && this.turn) || forceDown || (this.#p2Down && !this.turn)) {
            this.#down();
        }
        this.#resetKeys();
        this.#updateBoard();
    }

    #setPause() {
        if (this.#pressPause) {
            this.pause = !(this.pause);
        }
        this.#pressPause = false;
    }

    //avoid continuous firing of key
    #resetKeys() {
        this.#p1Right = false;
        this.#p2Right = false;
        this.#p1Left = false;
        this.#p2Left = false;
        this.#p1Down = false;
        this.#p2Down = false;
        this.#p1Rotate = false;
        this.#p2Rotate = false;
    }

    #checkCollision() {
        let movementOk = true;
        for (let i = 0; i < 4; i++) {
            for (let j = 0; j < 4; j++) {
                //for all the cell in the tetramino matrix that actually contains a block
                if (this.tetromino.matrix[i][j] == 1) {
                    //check if it will be displayed in a invalid position (out of screen or above another blocked tetramino)
                    if ((i + this.#tetraY < 0) || (i + this.#tetraY >= this.gameBoardH) || (j + this.#tetraX < 0) || (j + this.#tetraX >= this.gameBoardW)
                        || this.gameBoardMatrix[i + this.#tetraY][j + this.#tetraX] < 0) {
                        movementOk = false;
                    }
                }
            }
        }
        return movementOk;
    }

    #updateBoard() {
        this.#resetEmptySpaces();
        this.#placeTetramino();
    }

    #placeTetramino() {
        this.tetraminoPositionsMatrix = [];
        let moving = -1;
        if (this.#tetrominoMoving) {
            moving = 1;
        }
        for (let i = 0; i < 4; i++) {
            for (let j = 0; j < 4; j++) {
                if (this.tetromino.matrix[i][j] == 1) {
                    if ((i + this.#tetraY >= 0) && (i + this.#tetraY < this.gameBoardH) && (j + this.#tetraX >= 0) && (j + this.#tetraX < this.gameBoardW)) {
                        this.gameBoardMatrix[i + this.#tetraY][j + this.#tetraX] = this.tetromino.matrix[i][j] * this.tetromino.color * moving;
                        this.tetraminoPositionsMatrix.push([i + this.#tetraY, j + this.#tetraX]);
                    }
                }
                //test white for make visible the tetraminos' matix
                /*
                if(this.tetromino.matrix[i][j] == 0){
                    this.gameBoardMatrix[i + this.#tetraY][j + this.#tetraX] = 8;
                }
                */
            }
        }
    }

    // n < 0 : blocked; do not touch this
    // n = 0 : empty
    // n > 0 : moving tetramino
    #resetEmptySpaces() {
        for (let i = 0; i < this.gameBoardH; i++) {
            for (let j = 0; j < this.gameBoardW; j++) {
                if (this.gameBoardMatrix[i][j] > 0) {
                    this.gameBoardMatrix[i][j] = 0;
                }
            }
        }
    }

    #checkCompleteLine() {
        let complete = true;
        let lineCleared = 0;

        for (let i = this.gameBoardH - 1; i >= 0; i--) {
            for (let j = 0; j < this.gameBoardW & complete == true; j++) {
                if (this.gameBoardMatrix[i][j] >= 0) {
                    complete = false;
                }
            }
            if (complete) {
                this.gameBoardMatrix.splice(i, 1);
                this.gameBoardMatrix.unshift(Array(this.gameBoardW).fill(0));
                i++;
                lineCleared++;
            }
            complete = true;
        }

        if (lineCleared > 0) {
            this.#computeScore(this.turn, lineCleared)
        }
    }

    #computeScore(turn, lineCleared) {
        //bonus for one line = 0
        //bonus for two lines = 100
        //bonus for tree lines = 200
        //bonus for four lines = 300
        let bonus = 100 * lineCleared - 100;

        if (turn) {
            this.scoreP1 = this.scoreP1 + 100 * lineCleared + bonus;
        } else {
            this.scoreP2 = this.scoreP2 + 100 * lineCleared + bonus;
        }
    }

    #executeAiMoves() {
        //console.log(this.aiMoves);
        if (this.#readMoves) {
            if (this.aiMoves.length) {
                const move = this.aiMoves[0];
                //console.log(this.aiMoves);
                
                this.aiMoves.shift();
                switch (move) {
                    case 'rotate':
                        this.#p1Rotate = true;
                        break;
                    case 'left':
                        this.#p1Left = true;
                        break;
                    case 'right':
                        this.#p1Right = true;
                        break;
                    case 'down':
                        this.#p1Down = true;
                        break;
                }
            } else {
                //when the array of moves is empty
                //adding down commit the position and force the next tetromino to appear
                this.#p1Down = true;
                this.#readMoves = false;
            }            
        }
        
    }
}