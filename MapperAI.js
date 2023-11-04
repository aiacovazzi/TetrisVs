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
        this.#callApi('resetstart', 'GET', null);
        var apiPath;
        for (let i = 0; i < gB.currentAndNextTetramino.length; i++) {
            apiPath = 'start/' + this.#possibleShape[gB.currentAndNextTetramino[i]-1];
            console.log(apiPath);
            this.#callApi(apiPath, 'GET', null);
            }
    }

    #getPath(gB) {
        const data = this.#callApi('path/maxmax', 'GET', null);
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