"use strict";
/**
 * LSP server for vscode-ruby
 */
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) result[k] = mod[k];
    result["default"] = mod;
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_languageserver_1 = require("vscode-languageserver");
const rebuilder_1 = require("./util/rebuilder");
const connection = vscode_languageserver_1.createConnection(vscode_languageserver_1.ProposedFeatures.all);
let server;
connection.onInitialize((params) => __awaiter(this, void 0, void 0, function* () {
    connection.console.info('Initializing Ruby language server...');
    connection.console.info('Rebuilding tree-sitter for local Electron version');
    const rebuildResult = yield rebuilder_1.rebuildTreeSitter();
    for (const result of rebuildResult) {
        if (result) {
            connection.console.error('Rebuild failed!');
            connection.console.error(result.toString());
            return null;
        }
    }
    connection.console.info('Rebuild succeeded!');
    const { Server } = yield Promise.resolve().then(() => __importStar(require('./Server')));
    server = new Server(connection, params);
    server.registerInitializeProviders();
    return server.capabilities;
}));
connection.onInitialized(() => {
    server.registerInitializedProviders();
});
// Listen on the connection
connection.listen();
// Don't die on unhandled Promise rejections
process.on('unhandledRejection', (reason, p) => {
    connection.console.error(`Unhandled Rejection at: Promise ${p} reason:, ${reason}`);
});
// Don't die when attempting to pipe stdin to a bad spawn
// https://github.com/electron/electron/issues/13254
process.on('SIGPIPE', () => {
    // console.log('SIGPIPE!!');
});
//# sourceMappingURL=index.js.map