"use strict";
/*
 * DocumentHighlightProvider
 *
 * Super basic highlight provider
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
const Position_1 = __importDefault(require("../util/Position"));
const Provider_1 = __importDefault(require("./Provider"));
const DocumentHighlightAnalyzer_1 = __importDefault(require("../analyzers/DocumentHighlightAnalyzer"));
// TODO support more highlight use cases than just balanced pairs
class DocumentHighlightProvider extends Provider_1.default {
    constructor(connection) {
        super(connection);
        this.handleDocumentHighlight = (params) => __awaiter(this, void 0, void 0, function* () {
            const position = Position_1.default.fromVSPosition(params.position);
            const { textDocument: { uri }, } = params;
            return DocumentHighlightAnalyzer_1.default.analyze(uri, position);
        });
        this.connection.onDocumentHighlight(this.handleDocumentHighlight);
    }
    static register(connection) {
        return new DocumentHighlightProvider(connection);
    }
}
exports.default = DocumentHighlightProvider;
//# sourceMappingURL=DocumentHighlightProvider.js.map