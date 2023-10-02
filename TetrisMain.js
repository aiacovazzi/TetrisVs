import drawMenuCanvas from "./Menu.js";
import GameBoard from "./GameBoard.js";
import drawGameCanvas from "./Gui.js";
let canvas = document.getElementById('tetris');
let ctx = canvas.getContext('2d');
let option = null;
let gB;
let interval = setInterval(MenuLoop, 1000 / 60);
// Double the size of elements to fit the screen
ctx.scale(2, 2);

function MenuLoop(){
    if(option == null){
    option = drawMenuCanvas(ctx, canvas);
    }else{        
        clearInterval(interval);
        gB = new GameBoard(option);
        interval = setInterval(GameLoop, 1000 / 60);
    }
}

function GameLoop(){    
        gB.gameLoop();
        drawGameCanvas(ctx, canvas, gB);
}