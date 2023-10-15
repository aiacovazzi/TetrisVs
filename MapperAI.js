export default class MapperAI {
    #port = 7777;
    #possibleShape = ['t', 'z', 's', 'i', 'l', 'j', 'o'];

    async #callApi(apiPath) {
        try {
            const response = await fetch('http://localhost:' + this.#port + '/' + apiPath);
            if (!response.ok) {
                throw new Error('API request failed');
            }
            const data = await response.json();
            // Process the response data here 
            //console.log(data);
            return data;
        } catch (error) {
            // Handle any errors here 
            error.log(error);
            throw new Error(error);
        }
    }

    #assertOccupiedCell(gB) {
        var apiPath;
        this.#callApi('retcell');
        for (let i = 0; i < gB.gameBoardH; i++) {
            for (let j = 0; j < gB.gameBoardW; j++) {

                if (gB.gameBoardMatrix[i][j] < 0) {
                    apiPath = 'ocell/' + i + '/' + j
                    this.#callApi(apiPath);
                }
            }
        }
    }

    #assertTetraminos(gB) {
        var tetromino = this.#possibleShape[gB.tetromino.indexPiece] + '1/1/5';
        console.log(tetromino);
        this.#callApi('start/' + tetromino);
    }

    #getPath(gB) {
        const tetromino = this.#possibleShape[gB.tetromino.indexPiece]
        const data = this.#callApi('path/' + tetromino);
        data.then(function (result) {
            console.log(result);
            gB.aiMoves = result;
        })
    }

    getSolution(gB) {
        gB.aiMoves = [];
        this.#assertOccupiedCell(gB);
        this.#assertTetraminos(gB);
        this.#getPath(gB);

    }
}