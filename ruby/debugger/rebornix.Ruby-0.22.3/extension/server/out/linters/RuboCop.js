"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_uri_1 = __importDefault(require("vscode-uri"));
const vscode_languageserver_1 = require("vscode-languageserver");
const BaseLinter_1 = __importDefault(require("./BaseLinter"));
class RuboCop extends BaseLinter_1.default {
    constructor() {
        super(...arguments);
        this.code = 'RuboCop';
        this.DIAGNOSTIC_SEVERITIES = {
            refactor: vscode_languageserver_1.DiagnosticSeverity.Hint,
            convention: vscode_languageserver_1.DiagnosticSeverity.Information,
            warning: vscode_languageserver_1.DiagnosticSeverity.Warning,
            error: vscode_languageserver_1.DiagnosticSeverity.Error,
            fatal: vscode_languageserver_1.DiagnosticSeverity.Error,
        };
    }
    get cmd() {
        if (this.lintConfig.command) {
            return this.lintConfig.command;
        }
        else {
            const command = 'rubocop';
            return this.isWindows() ? command + '.bat' : command;
        }
    }
    get args() {
        const documentPath = vscode_uri_1.default.parse(this.document.uri);
        let args = ['-s', documentPath.fsPath, '-f', 'json'];
        if (this.lintConfig.rails)
            args.push('-R');
        if (this.lintConfig.forceExclusion)
            args.push('--force-exclusion');
        if (this.lintConfig.lint)
            args.push('-l');
        if (this.lintConfig.only)
            args = args.concat('--only', this.lintConfig.only.join(','));
        if (this.lintConfig.except)
            args = args.concat('--except', this.lintConfig.except.join(','));
        if (this.lintConfig.require)
            args = args.concat('-r', this.lintConfig.require.join(','));
        return args;
    }
    get lintConfig() {
        return this.config.config;
    }
    processResults(data) {
        let results = [];
        try {
            const offenses = JSON.parse(data);
            for (const file of offenses.files) {
                const diagnostics = file.offenses.map(o => this.rubocopOffenseToDiagnostic(o));
                results = results.concat(diagnostics);
            }
        }
        catch (e) {
            console.error(`Lint: Received invalid JSON from rubocop:\n\n${data}`);
        }
        return results;
    }
    rubocopOffenseToDiagnostic(offense) {
        const offenseCharacter = offense.location.column - 1;
        return {
            range: {
                start: {
                    line: offense.location.line - 1,
                    character: offenseCharacter,
                },
                end: {
                    line: offense.location.line - 1,
                    character: offenseCharacter + offense.location.length - 1,
                },
            },
            severity: this.DIAGNOSTIC_SEVERITIES[offense.severity],
            message: offense.message,
            source: offense.cop_name,
            code: this.code,
        };
    }
}
exports.default = RuboCop;
//# sourceMappingURL=RuboCop.js.map