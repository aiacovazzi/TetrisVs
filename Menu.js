let tetrisLogo = new Image(260, 90);
tetrisLogo.src = "images\\tetrisVS.png";
let tetrisBuilding = new Image(280, 280);
tetrisBuilding.src = "images\\building.png";
let aiExpEnabled = '◀ AI explanation: enabled ▶';
let aiExpDisabled = '◀ AI explanation: disabled ▶';
let aiLevelEasy = '◀ AI opponent level: easy ▶';
let aiLevelHard = '◀ AI opponent level: hard ▶';
let numberOfTurns = 1;
let vsModeNumberOfTurns = '◀ Vs Mode number of turns: '+numberOfTurns.toString() +' ▶';
let aiExp = aiExpDisabled;
let aiLevel = aiLevelEasy;
let optionAiExp = 'Disabled';
let optionAilevel = 'Easy';


let gameMode = [aiExp,aiLevel,vsModeNumberOfTurns,'Solo', 'Solo AI', 'Vs Player', 'Vs AI', 'AI vs AI'];
let options = null;
let index = 0;
let keydown = event => {

    if (event.code == "ArrowUp" || event.code == "KeyW") {
        if(index > 0){
            index --
        }
    }

    if (event.code == "ArrowDown" || event.code == "KeyS") {
        if(index < gameMode.length -1){
            index ++;
        }
    }

    if ((event.code == "ArrowRight" || event.code == "ArrowLeft" || event.code == "KeyA" || event.code == "KeyD") && index == 0)  {
        if(gameMode[0] == aiExpEnabled){
        gameMode[0] = aiExpDisabled;
        optionAiExp = 'Disabled';
        }else{
            gameMode[0] = aiExpEnabled;
            optionAiExp = 'Enabled';
        }
    }

    if ((event.code == "ArrowRight" || event.code == "ArrowLeft" || event.code == "KeyA" || event.code == "KeyD") && index == 1)  {
        if(gameMode[1] == aiLevelEasy){
        gameMode[1] = aiLevelHard;
        optionAilevel = 'Hard';
        }else{
            gameMode[1] = aiLevelEasy;
            optionAilevel = 'Easy';
        }
    }

    if ((event.code == "ArrowRight" || event.code == "KeyD") && index == 2)  {
        numberOfTurns = numberOfTurns + 1;
        if(numberOfTurns == 6){
            numberOfTurns = 1;
        }
        gameMode[2] = '◀ Vs Mode number of turns: '+numberOfTurns.toString() +' ▶';
    }

    
    if ((event.code == "ArrowLeft" || event.code == "KeyA") && index == 2)  {
        numberOfTurns = numberOfTurns - 1;
        if(numberOfTurns == 0){
            numberOfTurns = 5;
        }
        gameMode[2] = '◀ Vs Mode number of turns: '+numberOfTurns.toString() +' ▶';
    }

    if (event.code == "Enter" || event.code == "Space") {
        if(index == 3){
            options = [1,'Player','None',optionAiExp,optionAilevel,numberOfTurns];
        }else if(index == 4){
            options = [1,'AI','None',optionAiExp,optionAilevel,numberOfTurns];
        }else if(index == 5){
            options = [2,'Player','Player',optionAiExp,optionAilevel,numberOfTurns];
        }else if(index == 6){
            options = [2,'Player','AI',optionAiExp,optionAilevel,numberOfTurns];
        }else if(index == 7){
            options = [2,'AI','AI',optionAiExp,optionAilevel,numberOfTurns];
        }
    }

}
document.addEventListener("keydown", keydown);
export default function drawMenuCanvas(ctx, canvas){

    // Draw Canvas background
    ctx.fillStyle = 'black';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    //LOGO
    ctx.drawImage(tetrisBuilding, 285, 242, 260, 260);
    ctx.drawImage(tetrisLogo, 172, 90, 260, 90);

    ctx.fillStyle = 'white';
    ctx.font = '16px Arial'; 
    ctx.strokeStyle = 'white';
    ctx.lineWidth = 1;
   
    ctx.fillText("Select the game mode", 205, 185);

    let x = 205;
    let y = 250;
    for (let i = 0; i < gameMode.length; i++) {
        ctx.font = '14px Arial'; 
        ctx.fillStyle = 'white';
     
        if (i == index){
            ctx.font = 'bold 14px Arial'; 
            ctx.fillStyle = 'red'; 
        }
        ctx.fillText(gameMode[i], x, y);
        y = y + 20;
        
      } 
    
    return options;
}