"use strict";
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) result[k] = mod[k];
    result["default"] = mod;
    return result;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
/**
 * vscode-ruby main
 */
const vscode_1 = require("vscode");
const utils = __importStar(require("./utils"));
const languageConfiguration_1 = __importDefault(require("./languageConfiguration"));
const completion_1 = require("./providers/completion");
const configuration_1 = require("./providers/configuration");
const formatter_1 = require("./providers/formatter");
const highlight_1 = require("./providers/highlight");
const intellisense_1 = require("./providers/intellisense");
const linters_1 = require("./providers/linters");
const rake_1 = require("./task/rake");
const DOCUMENT_SELECTOR = [
    { language: 'ruby', scheme: 'file' },
    { language: 'ruby', scheme: 'untitled' },
];
let client;
function activate(context) {
    // register language config
    vscode_1.languages.setLanguageConfiguration('ruby', languageConfiguration_1.default);
    utils.getOutputChannel();
    if (vscode_1.workspace.getConfiguration('ruby').useLanguageServer) {
        client = require('../client/out/extension');
        client.activate(context);
    }
    else {
        // Register legacy providers
        highlight_1.registerHighlightProvider(context, DOCUMENT_SELECTOR);
        if (vscode_1.workspace.rootPath) {
            linters_1.registerLinters(context);
        }
    }
    // Register providers
    completion_1.registerCompletionProvider(context, DOCUMENT_SELECTOR);
    formatter_1.registerFormatter(context, DOCUMENT_SELECTOR);
    configuration_1.registerConfigurationProvider();
    if (vscode_1.workspace.rootPath) {
        intellisense_1.registerIntellisenseProvider(context);
        rake_1.registerTaskProvider(context);
    }
    utils.loadEnv();
}
exports.activate = activate;
function deactivate() {
    if (vscode_1.workspace.getConfiguration('ruby').useLanguageServer) {
        client.deactivate();
    }
    return undefined;
}
exports.deactivate = deactivate;
//# sourceMappingURL=ruby.js.map