export default class MapperAI {
    #port = 7777;
    #possibleShape = ['t', 'z', 's', 'i', 'l', 'j', 'o'];

    #callApi(apiPath, method, load) {
        const xhr = new XMLHttpRequest();
        xhr.open(method, 'http://localhost:' + this.#port + '/' + apiPath, false);
        xhr.send();
        if (xhr.status === 200) {
           return xhr.responseText;
        } else {
           throw new Error('Request failed: ' + xhr.statusText);
        }
     }

    #assertOccupiedCell(gB) {
        var apiPath;
        for (let i = 0; i < gB.tetraminoPositionsMatrix.length; i++) {
            apiPath = 'ocell/' + gB.tetraminoPositionsMatrix[i][0] + '/' + gB.tetraminoPositionsMatrix[i][1];
            console.log(apiPath);
            this.#callApi(apiPath, 'GET', null);
            }
        this.#callApi('newgb', 'GET', null);
    }

    #assertTetraminos(gB) {
        var tetromino = this.#possibleShape[gB.tetromino.indexPiece] + '1/1/5';
        this.#callApi('start/' + tetromino, 'GET', null);
    }

    #getPath(gB) {
        const tetromino = this.#possibleShape[gB.tetromino.indexPiece]
        const data = this.#callApi('path/' + tetromino, 'GET', null);
        gB.aiMoves = JSON.parse(data);
    }

    getSolution(gB) {
        gB.aiMoves = [];
        this.#assertOccupiedCell(gB);
        this.#assertTetraminos(gB);
        this.#getPath(gB);
    }
    reset(){
        this.#callApi('retcell', 'GET', null);
    }
}