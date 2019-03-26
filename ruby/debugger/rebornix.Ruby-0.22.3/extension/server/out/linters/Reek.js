"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const BaseLinter_1 = __importDefault(require("./BaseLinter"));
const vscode_languageserver_1 = require("vscode-languageserver");
class Reek extends BaseLinter_1.default {
    constructor() {
        super(...arguments);
        this.code = 'Reek';
    }
    get cmd() {
        if (this.lintConfig.command) {
            return this.lintConfig.command;
        }
        else {
            const command = 'reek';
            return this.isWindows() ? command + '.bat' : command;
        }
    }
    get args() {
        return ['-f', 'json'];
    }
    processResults(data) {
        let results = [];
        try {
            const offenses = JSON.parse(data);
            for (const offense of offenses) {
                const diagnostics = this.reekOffenseToDiagnostic(offense);
                results = results.concat(diagnostics);
            }
        }
        catch (e) {
            console.error(`Lint: Received invalid JSON from reek:\n\n${data}`);
        }
        return results;
    }
    reekOffenseToDiagnostic(offense) {
        const baseDiagnostic = {
            severity: vscode_languageserver_1.DiagnosticSeverity.Warning,
            message: offense.message,
            source: offense.smell_type,
            code: this.code,
        };
        return offense.lines.map(l => {
            return Object.assign({}, baseDiagnostic, { range: {
                    start: {
                        line: l,
                        character: 1,
                    },
                    end: {
                        line: l,
                        character: 1,
                    },
                } });
        });
    }
}
exports.default = Reek;
//# sourceMappingURL=Reek.js.map