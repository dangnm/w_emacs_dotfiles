"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_uri_1 = __importDefault(require("vscode-uri"));
const rxjs_1 = require("rxjs");
const operators_1 = require("rxjs/operators");
const SettingsCache_1 = require("./SettingsCache");
const linters_1 = require("./linters");
const DocumentManager_1 = require("./DocumentManager");
const LINTER_MAP = {
    rubocop: linters_1.RuboCop,
    reek: linters_1.Reek,
    standard: linters_1.Standard,
};
function getLinter(name, document, env, config) {
    const linter = LINTER_MAP[name];
    if (!linter)
        return new linters_1.NullLinter(name);
    const lintConfig = typeof config.lint[name] === 'object' ? config.lint[name] : {};
    const linterConfig = {
        env,
        executionRoot: vscode_uri_1.default.parse(config.workspaceFolderUri).fsPath,
        config: lintConfig,
    };
    return new linter(document, linterConfig);
}
function lint(document) {
    return rxjs_1.from(SettingsCache_1.documentConfigurationCache.get(document)).pipe(operators_1.mergeMap(config => SettingsCache_1.workspaceRubyEnvironmentCache.get(config.workspaceFolderUri), (config, env) => {
        return { config, env };
    }), operators_1.switchMap(({ config, env }) => {
        return rxjs_1.from(Object.keys(config.lint)).pipe(operators_1.mergeMap(l => {
            return config.lint[l] ? getLinter(l, document, env, config).lint() : rxjs_1.empty();
        }));
    }), operators_1.map(diagnostics => {
        return {
            document,
            diagnostics,
        };
    }));
}
exports.linter = DocumentManager_1.documents.subject.pipe(operators_1.switchMap((event) => rxjs_1.iif(() => event.kind === DocumentManager_1.DocumentEventKind.OPEN || event.kind === DocumentManager_1.DocumentEventKind.CHANGE_CONTENT, lint(event.document), rxjs_1.of({
    document: event.document,
    diagnostics: [],
}))));
//# sourceMappingURL=Linter.js.map