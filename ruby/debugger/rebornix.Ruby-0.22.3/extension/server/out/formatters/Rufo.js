"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_uri_1 = __importDefault(require("vscode-uri"));
const BaseFormatter_1 = __importDefault(require("./BaseFormatter"));
class Rubocop extends BaseFormatter_1.default {
    get cmd() {
        const command = 'rufo';
        return this.isWindows() ? command + '.bat' : command;
    }
    get args() {
        const documentPath = vscode_uri_1.default.parse(this.document.uri);
        return [`--filename=${documentPath.fsPath}`, '-x'];
    }
}
exports.default = Rubocop;
//# sourceMappingURL=Rufo.js.map