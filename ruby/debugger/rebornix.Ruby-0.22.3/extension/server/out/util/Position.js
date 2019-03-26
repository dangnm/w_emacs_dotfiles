"use strict";
/**
 * Position class
 *
 * This class supports converting to/from VSCode and TreeSitter positions
 */
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_languageserver_1 = require("vscode-languageserver");
class Position {
    constructor(row, col) {
        this.row = row;
        this.col = col;
    }
    static fromVSPosition(position) {
        return new Position(position.line, position.character);
    }
    static fromTSPosition(position) {
        return new Position(position.row, position.column);
    }
    static tsPositionIsEqual(p1, p2) {
        return p1.row === p2.row && p1.column === p2.column;
    }
    toVSPosition() {
        return vscode_languageserver_1.Position.create(this.row, this.col);
    }
    toTSPosition() {
        return {
            row: this.row,
            column: this.col,
        };
    }
    isEqual(position) {
        return this.row === position.row && this.col === position.col;
    }
}
exports.default = Position;
//# sourceMappingURL=Position.js.map