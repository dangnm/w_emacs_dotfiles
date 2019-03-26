"use strict";
/**
 * CapabilityCalculator
 */
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_languageserver_1 = require("vscode-languageserver");
class CapabilityCalculator {
    constructor(clientCapabilities) {
        this.clientCapabilities = clientCapabilities;
        this.calculateCapabilities();
    }
    calculateCapabilities() {
        this.capabilities = {
            // Perform incremental syncs
            // Incremental sync is disabled for now due to not being able to get the
            // old text
            // textDocumentSync: TextDocumentSyncKind.Incremental,
            textDocumentSync: vscode_languageserver_1.TextDocumentSyncKind.Full,
            documentFormattingProvider: true,
            documentRangeFormattingProvider: true,
            documentHighlightProvider: true,
            documentSymbolProvider: true,
            foldingRangeProvider: true,
        };
        if (this.clientCapabilities.workspace && this.clientCapabilities.workspace.workspaceFolders) {
            this.capabilities.workspace = {
                workspaceFolders: {
                    supported: true,
                    changeNotifications: true,
                },
            };
        }
    }
}
exports.CapabilityCalculator = CapabilityCalculator;
//# sourceMappingURL=CapabilityCalculator.js.map