export default class Tetromino{
//this class will generate a new tetramino and allows to manipulate it

//definition of all the possible pieces in every valid position as bitmask.
//see https://stackoverflow.com/questions/38594574/tetris-2d-array-logic
    #possibleShape = [
        [0x4C40, 0x0E40, 0x4640, 0x4E00], // 'T'
        [0x8C40, 0x6C00, 0x8C40, 0x6C00], // 'Z'
        [0x4C80, 0xC600, 0x4C80, 0xC600], // 'S'
        [0x4444, 0x0F00, 0x4444, 0x0F00], // 'I'
        [0x44C0, 0xE20 , 0x6440, 0x8E00], // 'L'
        [0xC440, 0xE80 , 0x4460, 0x2E00], // 'J'
        [0xCC00, 0xCC00, 0xCC00, 0xCC00]  // 'O'
    ];

    //each number is related to a different color that will be picked in the drawing phase
    #tetrominoColors = [1,2,3,4,5,6,7];

    #indexPosition = 0;

    indexPiece = 0;

    #piece = null;

    #position = null;

    matrix = [[0,0,0,0],
              [0,0,0,0],
              [0,0,0,0],
              [0,0,0,0]];
    
    constructor(){
        this.indexPiece = Math.floor((Math.random() * this.#possibleShape.length) );
        this.#piece = this.#possibleShape[this.indexPiece];
        this.color = this.#tetrominoColors[this.indexPiece];
        this.#position = this.#piece[this.#indexPosition];
        this.#drawPieceOnMatrix();
        //console.log(this.matrix);
    }

    rotate(){
        this.#rotation(1);
    }

    //allows to undo a rotation if it collide with pieces blocked in the board
    undoRotation(){
      this.#rotation(-1);
    }

    #rotation(direction){
      this.#indexPosition = (this.#indexPosition+direction)%4;

      //avoid bug when undo rotation starting from 3
      if(this.#indexPosition == -1){
        this.#indexPosition = 3;
      }

      this.#position = this.#piece[this.#indexPosition];
      this.#drawPieceOnMatrix();
    }

    //allows to convert bitmask in matrix
    #drawPieceOnMatrix(){
        for (var x = 0; x < 4; x++) {
            for (var y = 0; y < 4; y++) {
              if (this.#position & (0x8000 >> (y * 4 + x))) {
                this.matrix[x][y] = 1;
              }else{
                this.matrix[x][y] = 0;
              }
            }
          }
    }
}