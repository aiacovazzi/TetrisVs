let str='';
export default function mapGameBoard(gB, gameBoardW, gameBoardH) {
    for (var i = 0; i < gameBoardH; i++) {
        for (var j = 0; j < gameBoardW; j++) {
            if (gB[i][j] == 0) {
                str = str + 'cell(' + i + ',' + j + ',free).\n\r';
            } else {
                str = str + 'cell(' + i + ',' + j + ',occupied).\n\r';
            }
        }
    }
    console.log(str);
}
