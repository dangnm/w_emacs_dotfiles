"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_uri_1 = __importDefault(require("vscode-uri"));
const RuboCop_1 = __importDefault(require("./RuboCop"));
class Standard extends RuboCop_1.default {
    get cmd() {
        const command = 'standardrb';
        return this.isWindows() ? command + '.bat' : command;
    }
    get args() {
        const documentPath = vscode_uri_1.default.parse(this.document.uri);
        let args = ['-s', documentPath.fsPath, '--fix'];
        return args;
    }
}
exports.default = Standard;
//# sourceMappingURL=Standard.js.map