"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_languageclient_1 = require("vscode-languageclient");
const vscode_1 = require("vscode");
const env_1 = require("./util/env");
var WorkspaceRubyEnvironmentRequest;
(function (WorkspaceRubyEnvironmentRequest) {
    WorkspaceRubyEnvironmentRequest.type = new vscode_languageclient_1.RequestType('workspace/rubyEnvironment');
})(WorkspaceRubyEnvironmentRequest || (WorkspaceRubyEnvironmentRequest = {}));
class WorkspaceRubyEnvironmentFeature {
    constructor(client) {
        this.client = client;
    }
    fillClientCapabilities(capabilities) {
        capabilities.workspace = capabilities.workspace || {};
        capabilities.workspace.rubyEnvironment = true;
    }
    initialize() {
        this.client.onRequest(WorkspaceRubyEnvironmentRequest.type, (params) => __awaiter(this, void 0, void 0, function* () {
            const environments = {};
            for (const uri of params.folders) {
                const workspaceFolder = vscode_1.workspace.getWorkspaceFolder(vscode_1.Uri.parse(uri));
                if (workspaceFolder && workspaceFolder.uri.fsPath) {
                    environments[uri] = yield env_1.loadEnv(workspaceFolder.uri.fsPath);
                }
            }
            return environments;
        }));
    }
}
exports.WorkspaceRubyEnvironmentFeature = WorkspaceRubyEnvironmentFeature;
//# sourceMappingURL=WorkspaceRubyEnvironment.js.map