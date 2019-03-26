"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const RuboCop_1 = __importDefault(require("./RuboCop"));
class Standard extends RuboCop_1.default {
    constructor() {
        super(...arguments);
        this.code = 'Standard';
    }
    get cmd() {
        if (this.lintConfig.command) {
            return this.lintConfig.command;
        }
        else {
            const command = 'standardrb';
            return this.isWindows() ? command + '.bat' : command;
        }
    }
    // This method is overridden to deal with the "notice" that is
    // currently output
    processResults(data) {
        const lastCurly = data.lastIndexOf('}') + 1;
        let results = [];
        try {
            const offenses = JSON.parse(data.substring(0, lastCurly));
            for (const file of offenses.files) {
                const diagnostics = file.offenses.map(o => this.rubocopOffenseToDiagnostic(o));
                results = results.concat(diagnostics);
            }
        }
        catch (e) {
            console.error(`Lint: Received invalid JSON from standardrb:\n\n${data}`);
        }
        return results;
    }
}
exports.default = Standard;
//# sourceMappingURL=Standard.js.map