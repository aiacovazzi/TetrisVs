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
tetrisLogo.src = 'images\\tetrisVS.png';

export default function drawGameCanvas(ctx, canvas, gB){
    // Draw Canvas background
    ctx.fillStyle = 'black';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    
    // Draw gameboard
    drawGameBoard(gB, ctx, 38, 8, 280, 460);

    ctx.strokeStyle = 'white';
    ctx.lineWidth = 1;

    //LOGO
    ctx.drawImage(tetrisLogo, 330, 8, 210, 70);

    // Set font for score label text and draw for P1
    ctx.fillStyle = 'white';
    ctx.font = '12px Arial'; 

    //LEVEL
    ctx.fillText("STAGE", 330, 98);
    ctx.strokeRect(330, 107, 90, 24);  
    if(!gB.stopPlaying){ 
        levelText = gB.level +" / "+(60-gB.sec);
    }else{
        levelText = 'END';
    }

    ctx.fillText(levelText, 335, 125);  

    //TURN
    if(gB.playersNumber == 2){
        if(gB.turn){
            turn = 'P1';
        }else{
            turn = 'P2';
        }
    
    ctx.fillText("TURN", 430, 98);
    ctx.strokeRect(430, 107, 90, 24);     
    ctx.fillText(turn, 435, 125);   
    } 
   
    //SCORES P1
    ctx.fillText("SCORE P1", 330, 157);
    ctx.strokeRect(330, 171, 90, 24);
    ctx.fillText(gB.scoreP1, 335, 188);
    
    //NEXT P1
    ctx.fillText("NEXT PIECE P1", 330, 217);
    ctx.strokeRect(330, 231, 90, 90);
    if(!gB.getAiSolution){
        nextTetrominoP1.src = 'images\\tretrominoShapes\\'+tetrominoDrawings[gB.p1NextTetrominoIndex];
    }
    ctx.drawImage(nextTetrominoP1, 340, 250, 70, 50);
    
    if(gB.playerOne == 'Player'){
        //CONTROLS P1
        ctx.fillText("P1: CONTROLS", 330, 354);
        ctx.strokeRect(330, 364, 90, 104);
        ctx.font = '11.5px Arial';
        ctx.fillText("A: Move Left", 335, 388);
        ctx.fillText("D: Move Right", 335, 413);
        ctx.fillText("S: Move Down", 335, 438);
        ctx.fillText("SPACE: Rotate", 335, 463);
    }else{
        ctx.fillText("P1: AI", 330, 354);
        if(gB.getAiSolution && gB.turn){
            ctx.font = '8.2px Arial';
            ctx.fillText("Loading the next move...", 330, 375);
            ctx.font = '11.5px Arial';
        }
    }
    
    if(gB.playersNumber == 2){
        //SCORES P2
        ctx.fillText("SCORE P2", 430, 157);
        ctx.strokeRect(430, 171, 90, 24);
        ctx.fillText(gB.scoreP2, 435, 188);
        //NEXT P2
        ctx.fillText("NEXT PIECE P2", 430, 217);
        ctx.strokeRect(430, 231, 90, 90);
        if(!gB.getAiSolution){
            nextTetrominoP2.src = 'images\\tretrominoShapes\\'+tetrominoDrawings[gB.p2NextTetrominoIndex];
        }
        ctx.drawImage(nextTetrominoP2, 440, 250, 70, 50);

        if(gB.playerTwo == 'Player'){
            //CONTROLS P2
            ctx.fillText("P2: CONTROLS", 430, 354);
            ctx.strokeRect(430, 364, 90, 104);
            ctx.font = '11.5px Arial';
            ctx.fillText("←: Move Left", 435, 388);
            ctx.fillText("→: Move Right", 435, 413);
            ctx.fillText("↓: Move Down", 435, 438);
            ctx.fillText("CTRL: Rotate", 435, 463);
            }else{
                ctx.fillText("P2: AI", 430, 354);
                if(gB.getAiSolution && !gB.turn){
                    ctx.font = '8.2px Arial';
                    ctx.fillText("Loading the next move...", 430, 375);
                    ctx.font = '11.5px Arial';
                }
            }
            
    }
    //PAUSE
    if(gB.pause){
        if(gB.pauseExplain){
            endText = "Read the ai explanation in the console log";
            endTextX = 170;
            endTextY = 233;
        }else{
            endText = "Pause";
            endTextX = 225;
            endTextY = 238;
        }
    }

    //GAME ENDS
    if(gB.gameOver){
        endText = "Game Over!";
        endTextX = 198;
        endTextY = 238;
    }
    if(gB.p1Wins){
        endText = "Player One Wins!";
        endTextX = 173;
        endTextY = 238;
    }
    if(gB.p2Wins){
        endText = "Player Two Wins!";
        endTextX = 173;
        endTextY = 238;
    }
    if(gB.draw){
        endText = "It's A Draw!";
        endTextX = 202;
        endTextY = 238;
    }
    if(gB.stopPlaying || gB.pause){
        ctx.fillStyle = 'black';
        ctx.font = '22px Arial';
        if(gB.pauseExplain){
            ctx.font = '9px Arial';
        }
        ctx.globalAlpha = 0.5;
        ctx.fillRect(158, 195, 200, 70);
        ctx.globalAlpha = 1;
        ctx.fillStyle = 'white';
        ctx.strokeRect(158, 195, 200, 70);
        ctx.fillText(endText, endTextX, endTextY);
    }
    
}

function drawGameBoard(gB, ctx, x, y, width, height) {    
    let xStep = width / gB.gameBoardW;
    let yStep = height / gB.gameBoardH;
    let xCur = x;
    let yCur = y;
    !gB.emergency? ctx.strokeStyle = 'white' : ctx.strokeStyle = 'red';
    ctx.strokeRect(x, y, width, height);
    let fulcrum = 0;
    for (let i = 0; i < gB.gameBoardH; i++) {
        writeNumber(xCur-20,yCur+15,i,ctx);
        ctx.lineWidth = 0.1;
        for (let j = 0; j < gB.gameBoardW; j++) {
            fulcrum = Math.abs(gB.gameBoardMatrix[i][j])/10;
            if(fulcrum < 1){
                ctx.fillStyle = tetrominoColors[Math.abs(gB.gameBoardMatrix[i][j])];
                ctx.fillRect(xCur, yCur, xStep, yStep);
                ctx.strokeRect(xCur, yCur, xStep, yStep);
            }else{
                ctx.globalAlpha = 0.9;
                ctx.fillStyle = tetrominoColors[fulcrum];
                ctx.fillRect(xCur, yCur, xStep, yStep);
                ctx.strokeRect(xCur, yCur, xStep, yStep);
                ctx.globalAlpha = 1;
            }
            xCur = xCur + xStep;            

        }
        xCur = x;
        yCur = yCur + yStep;
    }

    for (let j = 0; j < gB.gameBoardW; j++) {
        writeNumber(xCur+10,yCur+15,j,ctx);
        xCur = xCur + xStep;            

    }

}

function writeNumber(x,y,number,ctx){
    ctx.fillStyle = 'white';
    ctx.font = '12px Arial'; 
    //ctx.strokeStyle = 'white';
    ctx.lineWidth = 1;
   
    ctx.fillText(number, x, y);
}