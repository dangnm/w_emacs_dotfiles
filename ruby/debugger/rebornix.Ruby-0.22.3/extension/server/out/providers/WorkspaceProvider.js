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
const Provider_1 = __importDefault(require("./Provider"));
const SettingsCache_1 = require("../SettingsCache");
class WorkspaceProvider extends Provider_1.default {
    constructor(connection) {
        super(connection);
        this.handleWorkspaceFoldersChange = (event) => __awaiter(this, void 0, void 0, function* () {
            const loader = SettingsCache_1.workspaceRubyEnvironmentCache.getAll(event.added);
            const remover = SettingsCache_1.workspaceRubyEnvironmentCache.deleteAll(event.removed);
            yield Promise.all([loader, remover]);
        });
        this.handleDidChangeWatchedFiles = (params) => __awaiter(this, void 0, void 0, function* () {
            console.log('Watched file change!');
            console.log(params);
            // TODO load workspace environment again based on workspace where the file changed
        });
        this.connection.workspace.onDidChangeWorkspaceFolders(this.handleWorkspaceFoldersChange);
        this.connection.onDidChangeWatchedFiles(this.handleDidChangeWatchedFiles);
    }
    static register(connection) {
        return new WorkspaceProvider(connection);
    }
}
exports.default = WorkspaceProvider;
//# sourceMappingURL=WorkspaceProvider.js.map