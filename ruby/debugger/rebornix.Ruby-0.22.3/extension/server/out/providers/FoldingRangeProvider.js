"use strict";
/*
 * FoldingRangeProvider
 */
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
const Analyzer_1 = require("../Analyzer");
class FoldingRangeProvider extends Provider_1.default {
    constructor(connection) {
        super(connection);
        this.handleFoldingRange = (params) => __awaiter(this, void 0, void 0, function* () {
            const { textDocument: { uri }, } = params;
            const analysis = Analyzer_1.analyses.getAnalysis(uri);
            return analysis.foldingRanges;
        });
        this.connection.onRequest(vscode_languageserver_1.FoldingRangeRequest.type, this.handleFoldingRange);
    }
    static register(connection) {
        return new FoldingRangeProvider(connection);
    }
}
exports.default = FoldingRangeProvider;
//# sourceMappingURL=FoldingRangeProvider.js.map