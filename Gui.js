//white is for visual test of the tetrominos' matrix
let tetrominoColors = ['black', 'purple', 'red', 'green', 'cyan', 'orange', 'blue', 'yellow','white'];
let tetrominoDrawings = ['','T.png', 'Z.png', 'S.png', 'I.png', 'L.png', 'J.png', 'O.png'];
let nextTetrominoP1 = new Image();
let nextTetrominoP2 = new Image();
let tetrisLogo = new Image(210, 70);
let turn = '';
let endText = '';
let levelText = '';
let endTextX = 0;
let endTextY = 0;
tetrisLogo.src = "images\\tetrisVS.png";
export default function drawGameCanvas(ctx, canvas, gB){
    // Draw Canvas background
    ctx.fillStyle = 'black';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    
    // Draw gameboard
    drawGameBoard(gB, ctx, 8, 8, 280, 460);

    ctx.strokeStyle = 'white';
    ctx.lineWidth = 1;

    //LOGO
    ctx.drawImage(tetrisLogo, 300, 8, 210, 70);

    // Set font for score label text and draw for P1
    ctx.fillStyle = 'white';
    ctx.font = '12px Arial'; 

    //LEVEL
    ctx.fillText("STAGE", 300, 98);
    ctx.strokeRect(300, 107, 90, 24);  
    if(!gB.stopPlaying){ 
        levelText = gB.level +" / "+(60-gB.sec);
    }else{
        levelText = 'END';
    }

    ctx.fillText(levelText, 305, 125);  

    //TURN
    if(gB.playersNumber == 2){
        if(gB.turn){
            turn = 'P1';
        }else{
            turn = 'P2';
        }
    
    ctx.fillText("TURN", 400, 98);
    ctx.strokeRect(400, 107, 90, 24);     
    ctx.fillText(turn, 405, 125);   
    } 
   
    //SCORES P1
    ctx.fillText("SCORE P1", 300, 157);
    ctx.strokeRect(300, 171, 90, 24);
    ctx.fillText(gB.scoreP1, 305, 188);
    

    //NEXT P1
    ctx.fillText("NEXT PIECE P1", 300, 217);
    ctx.strokeRect(300, 231, 90, 90);
    nextTetrominoP1.src = 'images\\tretrominoShapes\\'+tetrominoDrawings[gB.p1NextTetrominoIndex];
    ctx.drawImage(nextTetrominoP1, 310, 250, 70, 50);
    
    if(gB.playerOne == 'Player'){
        //CONTROLS P1
        ctx.fillText("P1: CONTROLS", 300, 354);
        ctx.strokeRect(300, 364, 90, 104);
        ctx.font = '11.5px Arial';
        ctx.fillText("A: Move Left", 305, 388);
        ctx.fillText("D: Move Right", 305, 413);
        ctx.fillText("S: Move Down", 305, 438);
        ctx.fillText("SPACE: Rotate", 305, 463);
    }else{
        ctx.fillText("P1: AI", 300, 354);
    }
    
    if(gB.playersNumber == 2){
        //SCORES P2
        ctx.fillText("SCORE P2", 400, 157);
        ctx.strokeRect(400, 171, 90, 24);
        ctx.fillText(gB.scoreP2, 405, 188);
        //NEXT P2
        ctx.fillText("NEXT PIECE P2", 400, 217);
        ctx.strokeRect(400, 231, 90, 90);
        nextTetrominoP2.src = 'images\\tretrominoShapes\\'+tetrominoDrawings[gB.p2NextTetrominoIndex];
        ctx.drawImage(nextTetrominoP2, 410, 250, 70, 50);

        if(gB.playerTwo == 'Player'){
            //CONTROLS P2
            ctx.fillText("P2: CONTROLS", 400, 354);
            ctx.strokeRect(400, 364, 90, 104);
            ctx.font = '11.5px Arial';
            ctx.fillText("←: Move Left", 405, 388);
            ctx.fillText("→: Move Right", 405, 413);
            ctx.fillText("↓: Move Down", 405, 438);
            ctx.fillText("CTRL: Rotate", 405, 463);
            }else{
                ctx.fillText("P2: AI", 400, 354);
            }
            
    }

    //GAME ENDS
    if(gB.gameOver){
        endText = "Game Over!";
        endTextX = 168;
        endTextY = 238;
    }
    if(gB.p1Wins){
        endText = "Player One Wins!";
        endTextX = 143;
        endTextY = 238;
    }
    if(gB.p2Wins){
        endText = "Player Two Wins!";
        endTextX = 143;
        endTextY = 238;
    }
    if(gB.draw){
        endText = "It's A Draw!";
        endTextX = 172;
        endTextY = 238;
    }
    if(gB.stopPlaying){
        ctx.fillStyle = 'black';
        ctx.font = '22px Arial';
        ctx.fillRect(128, 195, 200, 70);
        ctx.fillStyle = 'white';
        ctx.strokeRect(128, 195, 200, 70);
        ctx.fillText(endText, endTextX, endTextY);
    }
    
}

function drawGameBoard(gB, ctx, x, y, width, height) {    
    let xStep = width / gB.gameBoardW;
    let yStep = height / gB.gameBoardH;
    let xCur = x;
    let yCur = y;
    ctx.strokeStyle = 'white';
    ctx.strokeRect(x, y, width, height);
    ctx.lineWidth = 0.1;
    for (let i = 0; i < gB.gameBoardH; i++) {
        for (let j = 0; j < gB.gameBoardW; j++) {

            ctx.fillStyle = tetrominoColors[Math.abs(gB.gameBoardMatrix[i][j])];
            ctx.fillRect(xCur, yCur, xStep, yStep);
            ctx.strokeRect(xCur, yCur, xStep, yStep);
            xCur = xCur + xStep;            

        }
        xCur = x;
        yCur = yCur + yStep;
    }
}