let tetrisLogo = new Image(260, 90);
tetrisLogo.src = "images\\tetrisVS.png";
let tetrisBuilding = new Image(280, 280);
tetrisBuilding.src = "images\\building.png";
let gameMode = ['Solo', 'Solo AI', 'Vs Player', 'Vs AI', 'AI vs AI'];
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

    if (event.code == "Enter" || event.code == "Space") {
        if(index == 0){
            options = [1,'Player','None'];
        }else if(index == 1){
            options = [1,'AI','None'];
        }else if(index == 2){
            options = [2,'Player','Player'];
        }else if(index == 3){
            options = [2,'Player','AI'];
        }else if(index == 4){
            options = [2,'AI','AI'];
        }
    }

}
document.addEventListener("keydown", keydown);
export default function drawMenuCanvas(ctx, canvas){

    // Draw Canvas background
    ctx.fillStyle = 'black';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    //LOGO
    ctx.drawImage(tetrisBuilding, 240, 212, 260, 260);
    ctx.drawImage(tetrisLogo, 142, 90, 260, 90);

    ctx.fillStyle = 'white';
    ctx.font = '16px Arial'; 
    ctx.strokeStyle = 'white';
    ctx.lineWidth = 1;
   
    ctx.fillText("Select the game mode", 175, 185);

    let x = 175;
    let y = 220;
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