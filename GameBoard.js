import Tetromino from "./Tetromino.js";
import mapGameBoard from "./MapperAI.js";
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

    #tetromino = null;

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

    constructor(option) {
        var mov = this;
        this.playersNumber = option[0];
        this.playerOne = option[1];
        this.playerTwo = option[2];

        //assign the proper event listener for human player
        if(this.playerOne == 'Player'){
            document.addEventListener("keydown", this.#keydownP1);
        }
        if(this.playerTwo == 'Player'){
            document.addEventListener("keydown", this.#keydownP2);
        }

        this.#p1NextTetromino = new Tetromino();
        this.#p2NextTetromino = new Tetromino();
        this.p1NextTetrominoIndex = this.#p1NextTetromino.color;
        this.p2NextTetrominoIndex = this.#p2NextTetromino.color;
        this.#interval = setInterval(function () { mov.#move(true) }, mov.#intervalMs);
        this.#levelInterval = setInterval(function () { mov.#nextSec() }, 1000);
    }

    gameLoop() {
        if (!this.stopPlaying) {
            if (!this.#tetrominoMoving) {                
                this.#checkCompleteLine();
                this.#tetraX = this.#startX;
                this.#tetraY = this.#startY;
                this.#tetrominoMoving = true;

                //avoid to switch turn when in VS mode
                if (this.playersNumber == 2) {
                    this.turn = !this.turn;
                } else {
                    this.turn = true;
                }
                this.#assignNextTetromino(this.turn);
                this.gameOver = !(this.#checkCollision());

            } else {
                //then allows for player movement
                this.#move(false);
            }

        } else {
            this.#placeTetramino();
            clearInterval(this.#interval);
            clearInterval(this.#levelInterval);
        }

        this.stopPlaying = (this.gameOver || this.p1Wins || this.p2Wins || this.draw);
    }

    #nextSec() {
        this.sec = (this.sec + 1) % 60;
        if (this.sec == 0) {
            clearInterval(this.#interval);
            var mov = this;
            mov.#intervalMs = mov.#intervalMs * 0.8;
            this.#interval = setInterval(function () { mov.#move(true) }, mov.#intervalMs);
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
            this.#tetromino = Object.assign(this.#p1NextTetromino);
            this.#p1NextTetromino = new Tetromino();
            this.p1NextTetrominoIndex = this.#p1NextTetromino.color;
        } else {
            this.#tetromino = Object.assign(this.#p2NextTetromino);
            this.#p2NextTetromino = new Tetromino();
            this.p2NextTetrominoIndex = this.#p2NextTetromino.color;
        }
    }

    #move(forceDown) {
        if ((this.#p1Rotate && this.turn) || (this.#p2Rotate && !this.turn)) {
            this.#tetromino.rotate();
            //check if rotation is legit
            if (!this.#checkCollision()) {
                this.#tetromino.undoRotation();
            }

        }
        if ((this.#p1Left && this.turn) || (this.#p2Left && !this.turn)) {
            this.#tetraX = this.#tetraX - 1;
            //check if left movement is legit
            if (!this.#checkCollision()) {
                this.#tetraX = this.#tetraX + 1;
            }
        }
        if ((this.#p1Right && this.turn) || (this.#p2Right && !this.turn)) {
            this.#tetraX = this.#tetraX + 1;
            //check if right movement is legit
            if (!this.#checkCollision()) {
                this.#tetraX = this.#tetraX - 1;
            }
        }
        if ((this.#p1Down && this.turn) || forceDown || (this.#p2Down && !this.turn)) {
            this.#tetraY = this.#tetraY + 1;
            //check if down movement is legit
            //non legit down movement blocks the tetramino on the board
            if (!this.#checkCollision()) {
                this.#tetrominoMoving = false;
                this.#tetraY = this.#tetraY - 1;
            }
        }
        this.#resetKeys();
        this.#updateBoard();
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
                if (this.#tetromino.matrix[i][j] == 1) {
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
        let moving = -1;
        if (this.#tetrominoMoving) {
            moving = 1;
        }
        for (let i = 0; i < 4; i++) {
            for (let j = 0; j < 4; j++) {
                if (this.#tetromino.matrix[i][j] == 1) {
                    if ((i + this.#tetraY >= 0) && (i + this.#tetraY < this.gameBoardH) && (j + this.#tetraX >= 0) && (j + this.#tetraX < this.gameBoardW)) {
                        this.gameBoardMatrix[i + this.#tetraY][j + this.#tetraX] = this.#tetromino.matrix[i][j] * this.#tetromino.color * moving;
                    }
                }
                //test white for make visible the tetraminos' matix
                /*
                if(this.#tetromino.matrix[i][j] == 0){
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

    
}