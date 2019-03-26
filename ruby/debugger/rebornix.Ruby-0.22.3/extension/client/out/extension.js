"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
/**
 * LSP client for vscode-ruby
 */
const path_1 = __importDefault(require("path"));
const vscode_1 = require("vscode");
const vscode_languageclient_1 = require("vscode-languageclient");
const WorkspaceRubyEnvironment_1 = require("./WorkspaceRubyEnvironment");
const RUBOCOP_ABSOLUTE_PATH_KEYS = ['only', 'except', 'require'];
let client;
function activate(context) {
    const serverModule = context.asAbsolutePath(path_1.default.join('server', 'out', 'index.js'));
    const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    const serverOptions = {
        run: { module: serverModule, transport: vscode_languageclient_1.TransportKind.ipc },
        debug: { module: serverModule, transport: vscode_languageclient_1.TransportKind.ipc, options: debugOptions },
    };
    const rubyDocumentSelector = [
        { scheme: 'file', language: 'ruby' },
        { scheme: 'untitled', language: 'ruby' },
    ];
    // Options to control the language client
    const clientOptions = {
        documentSelector: rubyDocumentSelector,
        synchronize: {
            // Notify server of changes to .ruby-version or .rvmrc files
            fileEvents: vscode_1.workspace.createFileSystemWatcher('**/{.ruby-version,.rvmrc}'),
        },
        outputChannel: vscode_1.window.createOutputChannel('Ruby Language Server'),
        middleware: {
            workspace: {
                configuration: (params, token, next) => {
                    if (!params.items) {
                        return [];
                    }
                    let result = next(params, token, next);
                    let settings = result[0];
                    let scopeUri = '';
                    for (let item of params.items) {
                        if (!item.scopeUri) {
                            continue;
                        }
                        else {
                            scopeUri = item.scopeUri;
                        }
                    }
                    let resource = client.protocol2CodeConverter.asUri(scopeUri);
                    let workspaceFolder = vscode_1.workspace.getWorkspaceFolder(resource);
                    if (workspaceFolder) {
                        // Convert any relative paths to absolute paths
                        if (settings.lint &&
                            settings.lint.rubocop &&
                            typeof settings.lint.rubocop === 'object') {
                            const { lint: { rubocop }, } = settings;
                            for (const key of RUBOCOP_ABSOLUTE_PATH_KEYS) {
                                if (rubocop[key]) {
                                    rubocop[key] = rubocop[key].map(f => convertAbsolute(f, workspaceFolder));
                                }
                            }
                        }
                        // Save the file's workspace folder
                        const protocolUri = client.code2ProtocolConverter.asUri(workspaceFolder.uri);
                        settings.workspaceFolderUri = protocolUri;
                    }
                    return result;
                },
            },
        },
    };
    // Create the language client and start the client.
    client = new vscode_languageclient_1.LanguageClient('ruby', 'Ruby', serverOptions, clientOptions);
    client.registerProposedFeatures();
    client.registerFeature(new WorkspaceRubyEnvironment_1.WorkspaceRubyEnvironmentFeature(client));
    // Push the disposable to the context's subscriptions so that the
    // client can be deactivated on extension deactivation
    context.subscriptions.push(client.start());
}
exports.activate = activate;
function deactivate() {
    return client ? client.stop() : undefined;
}
exports.deactivate = deactivate;
function convertAbsolute(file, folder) {
    if (path_1.default.isAbsolute(file)) {
        return file;
    }
    let folderPath = folder.uri.fsPath;
    if (!folderPath) {
        return file;
    }
    return path_1.default.join(folderPath, file);
}
//# sourceMappingURL=extension.js.map