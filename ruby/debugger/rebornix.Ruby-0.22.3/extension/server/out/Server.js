"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_languageserver_1 = require("vscode-languageserver");
const CapabilityCalculator_1 = require("./CapabilityCalculator");
const DocumentHighlightProvider_1 = __importDefault(require("./providers/DocumentHighlightProvider"));
const FoldingRangeProvider_1 = __importDefault(require("./providers/FoldingRangeProvider"));
const ConfigurationProvider_1 = __importDefault(require("./providers/ConfigurationProvider"));
const WorkspaceProvider_1 = __importDefault(require("./providers/WorkspaceProvider"));
const DocumentSymbolProvider_1 = __importDefault(require("./providers/DocumentSymbolProvider"));
const DocumentManager_1 = require("./DocumentManager");
const Linter_1 = require("./Linter");
const SettingsCache_1 = require("./SettingsCache");
const DocumentFormattingProvider_1 = __importDefault(require("./providers/DocumentFormattingProvider"));
var WorkspaceRubyEnvironmentRequest;
(function (WorkspaceRubyEnvironmentRequest) {
    WorkspaceRubyEnvironmentRequest.type = new vscode_languageserver_1.RequestType('workspace/rubyEnvironment');
})(WorkspaceRubyEnvironmentRequest || (WorkspaceRubyEnvironmentRequest = {}));
class Server {
    constructor(connection, params) {
        this.connection = connection;
        this.calculator = new CapabilityCalculator_1.CapabilityCalculator(params.capabilities);
        DocumentManager_1.documents.listen(connection);
        Linter_1.linter.subscribe({
            next: (result) => {
                connection.sendDiagnostics({ uri: result.document.uri, diagnostics: result.diagnostics });
            },
        });
        SettingsCache_1.documentConfigurationCache.fetcher = (targets) => __awaiter(this, void 0, void 0, function* () {
            const items = targets.map(t => {
                return {
                    scopeUri: t,
                    section: 'ruby',
                };
            });
            return this.connection.workspace.getConfiguration(items);
        });
        if (this.calculator.clientCapabilities.workspace.rubyEnvironment) {
            SettingsCache_1.workspaceRubyEnvironmentCache.fetcher = (folders) => __awaiter(this, void 0, void 0, function* () {
                const result = yield this.connection.sendRequest(WorkspaceRubyEnvironmentRequest.type, {
                    folders,
                });
                return Object.values(result);
            });
        }
        else {
            SettingsCache_1.workspaceRubyEnvironmentCache.fetcher = (folders) => __awaiter(this, void 0, void 0, function* () {
                return folders.map(_f => process.env);
            });
        }
    }
    get capabilities() {
        return {
            capabilities: this.calculator.capabilities,
        };
    }
    // registers providers on the initialize step
    registerInitializeProviders() {
        // Handles highlight requests
        DocumentHighlightProvider_1.default.register(this.connection);
        // Handles folding requests
        FoldingRangeProvider_1.default.register(this.connection);
        // Handles document symbol requests
        DocumentSymbolProvider_1.default.register(this.connection);
        // Handles document formatting requests
        DocumentFormattingProvider_1.default.register(this.connection);
    }
    // registers providers on the initialized step
    registerInitializedProviders() {
        // Handles configuration changes
        ConfigurationProvider_1.default.register(this.connection);
        // Handle workspace changes
        WorkspaceProvider_1.default.register(this.connection);
    }
}
exports.Server = Server;
//# sourceMappingURL=Server.js.map