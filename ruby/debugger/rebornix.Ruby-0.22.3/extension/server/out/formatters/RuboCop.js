"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_uri_1 = __importDefault(require("vscode-uri"));
const BaseFormatter_1 = __importDefault(require("./BaseFormatter"));
class RuboCop extends BaseFormatter_1.default {
    constructor() {
        super(...arguments);
        this.FORMATTED_OUTPUT_DELIMITER = '====================';
    }
    get cmd() {
        const command = 'rubocop';
        return this.isWindows() ? command + '.bat' : command;
    }
    get args() {
        const documentPath = vscode_uri_1.default.parse(this.document.uri);
        let args = ['-s', documentPath.fsPath, '-a'];
        return args;
    }
    processResults(output) {
        const endOfDiagnostics = output.lastIndexOf(this.FORMATTED_OUTPUT_DELIMITER) + this.FORMATTED_OUTPUT_DELIMITER.length;
        const cleanOutput = output.substring(endOfDiagnostics).trimLeft();
        return super.processResults(cleanOutput);
    }
    processError(error, formatStr) {
        let code = error.code || error.toString().match(/code: (\d+)/)[1] || null;
        if (code === '1')
            return null;
        return super.processError(error, formatStr);
    }
}
exports.default = RuboCop;
//# sourceMappingURL=RuboCop.js.map