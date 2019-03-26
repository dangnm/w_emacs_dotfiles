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
const Provider_1 = __importDefault(require("./Provider"));
const SettingsCache_1 = require("../SettingsCache");
class ConfigurationProvider extends Provider_1.default {
    constructor(connection) {
        super(connection);
        this.handleDidChangeConfiguration = (_params // params is empty in the pull config model
        ) => __awaiter(this, void 0, void 0, function* () {
            SettingsCache_1.documentConfigurationCache.flush();
        });
        this.connection.client.register(vscode_languageserver_1.DidChangeConfigurationNotification.type, undefined);
        this.connection.onDidChangeConfiguration(this.handleDidChangeConfiguration);
    }
    static register(connection) {
        return new ConfigurationProvider(connection);
    }
}
exports.default = ConfigurationProvider;
//# sourceMappingURL=ConfigurationProvider.js.map