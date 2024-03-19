//"p1 wins" even if p2 win!
import Tetromino from "./Tetromino.js";
import MapperAI from "./MapperAI.js";
export default class GameBoard {
    #theme = null;
    gameBoardW = 10;
    gameBoardH = 20;

    #startX = 4;
    #startY = 0;

    tetraX = 0;
    tetraY = 0;

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

    getAiSolution = false;
    #aiMapper = null;
    #aiEnabled = false;
    #readMoves = false;
    aiMoves = [];
    currentAndNextTetramino = [];
    #aiTurns = [];
    aiPlayer = null;

    #explainMode = true;
    minMaxExplanation = null;
    explanationText = null;
    pauseExplain = null;

    emergency = false;

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
        option[3] == 'Enabled' ? this.#explainMode = true: this.#explainMode = false;

        //assign the proper event listener for human player
        if (this.playerOne == 'Player') {
            document.addEventListener("keydown", this.#keydownP1);
        }else{
            this.#aiTurns.push(true);
        }

        if (this.playerTwo == 'Player') {
            document.addEventListener("keydown", this.#keydownP2);
        }else{
            this.#aiTurns.push(false);
        }

        //enable AI
        if (this.playerOne == 'AI' || this.playerTwo == 'AI') {
            this.#aiEnabled = true;        
        }

        if (this.#aiEnabled && !this.gameOver) {
            this.#aiMapper.reset();
        }

        document.addEventListener("keydown", this.#keydownPause);

        if (this.playerOne != 'None') {
            this.#p1NextTetromino = new Tetromino();
            this.p1NextTetrominoIndex = this.#p1NextTetromino.color;
        }

        if (this.playerTwo != 'None') {
            this.#p2NextTetromino = new Tetromino();
            this.p2NextTetrominoIndex = this.#p2NextTetromino.color;
        }

        this.#setSong();        
        this.#setIntervalMovement(this.#intervalMs);
        this.#levelInterval = setInterval(function () { if (!mov.pause) { mov.#nextSec() } }, 1000);
    }

    #setSong(){
        this.playersNumber == 1 ? this.#theme = new Audio('sounds\\themealoop.mp3') : this.#theme = new Audio('sounds\\themebloop.mp3');
        this.#theme.loop = true;
        this.#theme.volume = 0;
        this.#theme.play();
    }

    gameLoop() {
        if (!this.stopPlaying) {
            if (!this.pause) {
                if (!this.#tetrominoMoving) {
                    this.#checkCompleteLine();
                    this.#setEmergencyMode();
                    if(this.#aiEnabled && !this.gameOver && !this.#isAiTurn()){
                        this.#aiMapper.assertBoard(this);
                    }
                    //switch turn when in VS mode, avoid it elsewhere
                    if (this.playersNumber == 2) {
                        this.turn = !this.turn;
                    } else {
                        this.turn = true;
                    }

                    this.tetraX = this.#startX;
                    this.tetraY = this.#startY;
                   
                    this.#assignNextTetromino(this.turn);
                    
                    this.gameOver = !(this.#checkCollision());
                    if(this.#aiEnabled && !this.gameOver && this.#isAiTurn()){
                        this.#prepareTetraminosArray(this.turn);
                        this.getAiSolution = true;
                    }

                    this.#tetrominoMoving = true; 
                } else {
                    
                    if(this.getAiSolution == true){
                        this.playersNumber == 1 || this.emergency? this.aiPlayer = 'maxmax' : this.aiPlayer = 'max';
                        //remove last tetromino if emergecy mode in vs mode
                        this.aiPlayer == 'maxmax' && this.playersNumber == 2? this.currentAndNextTetramino.pop() : null; 
                        console.log(this.aiPlayer +' '+this.currentAndNextTetramino);
                        this.#aiMapper.getSolution(this);
                        if(this.#explainMode){
                            this.#explain();
                        }
                        this.getAiSolution = false;
                    }

                    if (this.#isAiTurn() && !this.pause) {
                        this.#readMoves = true;
                        this.#executeAiMoves();
                    }  

                    //then allows for player movement
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
        this.#interval = setInterval(function () { if (!mov.pause && !mov.#isAiTurn()) { mov.#move(true) } }, mov.#intervalMs);
    }

    #isAiTurn(){
        return this.#aiTurns.includes(this.turn);
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
                if (this.scoreP1 > this.scoreP2) {
                    this.p1Wins = true;
                } else if (this.scoreP1 < this.scoreP2) {
                    this.p2Wins = true;
                } else if (this.scoreP1 == this.scoreP2) {
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

    #prepareTetraminosArray(turn) {
        //single player
        if (this.playerTwo == 'None') {
            this.currentAndNextTetramino = [this.tetromino.color, this.#p1NextTetromino.color];
        } else {
            if (turn) {
                this.currentAndNextTetramino = [this.tetromino.color, this.#p2NextTetromino.color, this.#p1NextTetromino.color];
            } else {
                this.currentAndNextTetramino = [this.tetromino.color, this.#p1NextTetromino.color, this.#p2NextTetromino.color];
            }
        }
    }
    
    #rotate() {
        this.tetromino.rotate();
    }

    #undoRotation() {
        this.tetromino.undoRotation();
    }

    #left() {
        this.tetraX = this.tetraX - 1;
    }

    #right() {
        this.tetraX = this.tetraX + 1;
    }

    #down() {
        this.tetraY = this.tetraY + 1;
        //check if down movement is legit
        //non legit down movement blocks the tetramino on the board
        if (!this.#checkCollision()) {
            this.#tetrominoMoving = false;
            this.#undoDown();
        }
    }

    #undoDown() {
        this.tetraY = this.tetraY - 1;
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
            this.pauseExplain = false;
            this.pause ? this.#theme.volume = 0.1 : this.#theme.volume = 1.0;
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
                    if ((i + this.tetraY < 0) || (i + this.tetraY >= this.gameBoardH) || (j + this.tetraX < 0) || (j + this.tetraX >= this.gameBoardW)
                        || this.gameBoardMatrix[i + this.tetraY][j + this.tetraX] < 0) {
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
        let fulcrum = 1;
        if (this.#tetrominoMoving) {
            moving = 1;
        }
        for (let i = 0; i < 4; i++) {
            for (let j = 0; j < 4; j++) {
                if (this.tetromino.matrix[i][j] == 1) {
                    if ((i + this.tetraY >= 0) && (i + this.tetraY < this.gameBoardH) && (j + this.tetraX >= 0) && (j + this.tetraX < this.gameBoardW)) {
                        if(i == 1 && j == 1){
                            fulcrum = 10;
                        }else{
                            fulcrum = 1;
                        }
                        this.gameBoardMatrix[i + this.tetraY][j + this.tetraX] = this.tetromino.matrix[i][j] * this.tetromino.color * moving * fulcrum;
                    }
                }
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
        if (this.#readMoves) {
            if (this.aiMoves.length) {
                const move = this.aiMoves[0];
                this.aiMoves.shift();
                this.#aiAction(move,this.turn);
            } else {
                //when the array of moves is empty
                //adding down commit the position and force the next tetromino to appear
                this.#aiAction('down',this.turn)
                this.#readMoves = false;
            }
        }

    }

    #aiAction(action,turn){
        switch (action) {
                    case 'rotate':
                        turn ? this.#p1Rotate = true: this.#p2Rotate = true;                        
                        break;
                    case 'left':
                        turn ? this.#p1Left = true: this.#p2Left = true; 
                        break;
                    case 'right':
                        turn ? this.#p1Right = true: this.#p2Right = true;
                        break;
                    case 'down':
                        turn ? this.#p1Down = true: this.#p2Down = true;
                        break;
                }
    }

    #explain(){
        this.pauseExplain = true;
        this.pause = true;
        console.clear();    
        this.explanationText = 'The information about the tetrominoes position are given using the following structure: [TetrominoCode Row Column].\n\rThe tetromino code represent the kind of tetromino along with its rotation state as a number between 1 and 4, row and column represent the coordinate of the tetromino. The coordinate are relative to the fulcrum that is the darker square of each tetromino.\n\r';
        this.explanationText = this.explanationText + 'To understand how to translate the tetramino code into the actual shape, you can refer the guide reachable using the following hyperlink: http://localhost:5500/images/tetrominoesExplained.png.\n\r\n\r'
          
        if(this.playersNumber == 1 || this.emergency){
            if(this.emergency){
                this.explanationText = this.explanationText + 'The gameboard is almost full! So it\'s time to collaborate to avoid game over. In order to do so, the ai will play helping to clear the board without consider any countermove.\n\rThe next move is computed looking foward only to the other player move to enhance performance.\n\r';
            }
            this.explanationText = this.explanationText + 'The next move is ['+this.minMaxExplanation[0][0].toUpperCase()+' '+this.minMaxExplanation[0][1]+' '+this.minMaxExplanation[0][2]+'] because it allows to reach, in the next step, ['+this.minMaxExplanation[1][0].toUpperCase()+' '+this.minMaxExplanation[1][1]+' '+this.minMaxExplanation[1][2]+'] that maximize the player advantage (this is true unless a better move will be available the next step).\n\r';
            this.explanationText = this.explanationText + 'To understand why ['+this.minMaxExplanation[1][0].toUpperCase()+' '+this.minMaxExplanation[1][1]+' '+this.minMaxExplanation[1][2]+'] is the best move to reach, read the heuristic evaluation of that move by clicking the following hyperlink: http://localhost:7777/explainHeuristic.\n\r';
        }else{
            var player = null;
            var adversary = null;
            
            if(this.turn){
                player = 'P1';
                adversary = 'P2';
            }else{
                player = 'P2';
                adversary = 'P1';}
            console.log(this.minMaxExplanation.length);
            if(this.minMaxExplanation.length > 1){
                this.explanationText = this.explanationText + 'The next move for '+player+' is ['+this.minMaxExplanation[0][0].toUpperCase()+' '+this.minMaxExplanation[0][1]+' '+this.minMaxExplanation[0][2]+'], this because it allows, in two step, to reach ['+this.minMaxExplanation[2][0].toUpperCase()+' '+this.minMaxExplanation[2][1]+' '+this.minMaxExplanation[2][2]+'] that is the move that maximize '+player+' advantage considering that '+adversary+' has ['+this.minMaxExplanation[1][0].toUpperCase()+' '+this.minMaxExplanation[1][1]+' '+this.minMaxExplanation[1][2]+'] as best countermove (this is true unless a better move will be available the next step).\n\r'
                this.explanationText = this.explanationText + 'To understand why ['+this.minMaxExplanation[2][0].toUpperCase()+' '+this.minMaxExplanation[2][1]+' '+this.minMaxExplanation[2][2]+'] is the best move to reach, read the heuristic evaluation of that move by clicking the following hyperlink: http://localhost:7777/explainHeuristic.\n\r';    
            }else{
                this.explanationText = this.explanationText + 'The next move for '+player+' is ['+this.minMaxExplanation[0][0].toUpperCase()+' '+this.minMaxExplanation[0][1]+' '+this.minMaxExplanation[0][2]+'], that is the move that maximize '+player+' advantage clearing some rows.\n\r' 
                this.explanationText = this.explanationText + 'To understand why ['+this.minMaxExplanation[0][0].toUpperCase()+' '+this.minMaxExplanation[0][1]+' '+this.minMaxExplanation[0][2]+'] is the best move to reach, read the heuristic evaluation of that move by clicking the following hyperlink: http://localhost:7777/explainHeuristic.\n\r';    
            }
            
        }

        this.explanationText = this.explanationText + '\n\rThe path for reach the wanted position is: ['+this.aiMoves+'].\n\rTo see the path history along with the discarded path click the following hyperlink: http://localhost:7777/explainPath.'
        console.log(this.explanationText);
    }

    #setEmergencyMode() {
        let min = this.gameBoardH;
        let nextColumn = false;
        for (let i = 0; i < this.gameBoardH; i++) {
            nextColumn = false;
            for (let j = 0; j < this.gameBoardW && nextColumn == false; j++) { 

                if(this.gameBoardMatrix[i][j] < 0 && i < min){
                    min = i;
                    nextColumn = true;
                }
            }
        }
        min <= 6? this.emergency = true : this.emergency =false;
    }
}