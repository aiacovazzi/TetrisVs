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

    #assertPlacedTetramino(gB) {
        var apiPath;
        apiPath = 'put/' + this.#possibleShape[gB.tetromino.color-1] + (gB.tetromino.indexPosition+1) + '/'+ (gB.tetraY+1) + '/' +(gB.tetraX+1)
        this.#callApi(apiPath, 'GET', null);
    }

    #assertStartingTetraminos(gB) {
        this.#callApi('resetstart', 'GET', null);
        var apiPath;
        for (let i = 0; i < gB.currentAndNextTetramino.length; i++) {
            apiPath = 'start/' + this.#possibleShape[gB.currentAndNextTetramino[i]-1];
            this.#callApi(apiPath, 'GET', null);
            }
    }

    #getPath(gB) {
        const data = this.#callApi('path/'+gB.aiPlayer, 'GET', null);
        gB.aiMoves = JSON.parse(data);
    }

    #getMinMaxExplanation(gB) {
        const data = this.#callApi('explainMove', 'GET', null);
        gB.minMaxExplanation = JSON.parse(data);
    }

    getSolution(gB) {
        gB.aiMoves = [];
        this.#assertStartingTetraminos(gB);
        this.#getPath(gB);
        this.#getMinMaxExplanation(gB);
    }

    assertBoard(gB) {
        this.#assertPlacedTetramino(gB);
        //this.#assertStartingTetraminos(gB);
    }

    reset(gB){
        this.#callApi('retcell', 'GET', null);
        this.#callApi('resetstart', 'GET', null);
        if(gB.easyMode){
            this.#callApi('easyMode', 'GET', null);
        }else{
            this.#callApi('hardMode', 'GET', null);
        }
    }
}