"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const SettingsCache_1 = require("./SettingsCache");
const DocumentManager_1 = require("./DocumentManager");
const vscode_uri_1 = __importDefault(require("vscode-uri"));
const rxjs_1 = require("rxjs");
const operators_1 = require("rxjs/operators");
const formatters_1 = require("./formatters");
const FORMATTER_MAP = {
    rubocop: formatters_1.RuboCop,
    standard: formatters_1.Standard,
    rufo: formatters_1.Rufo,
};
function getFormatter(document, env, config, range) {
    if (typeof config.format === 'string') {
        const formatterConfig = {
            env,
            executionRoot: vscode_uri_1.default.parse(config.workspaceFolderUri).fsPath,
            config: {
                command: config.format,
                useBundler: config.useBundler,
            },
        };
        if (range) {
            formatterConfig.range = range;
        }
        return new FORMATTER_MAP[config.format](document, formatterConfig);
    }
    else {
        return new formatters_1.NullFormatter();
    }
}
class Formatter {
    static format(ident, range) {
        const document = DocumentManager_1.documents.get(ident.uri);
        return rxjs_1.from(SettingsCache_1.documentConfigurationCache.get(ident.uri)).pipe(operators_1.switchMap(config => rxjs_1.from(SettingsCache_1.workspaceRubyEnvironmentCache.get(config.workspaceFolderUri)).pipe(operators_1.switchMap(env => {
            return getFormatter(document, env, config, range).format();
        }))));
    }
}
exports.default = Formatter;
//# sourceMappingURL=Formatter.js.map