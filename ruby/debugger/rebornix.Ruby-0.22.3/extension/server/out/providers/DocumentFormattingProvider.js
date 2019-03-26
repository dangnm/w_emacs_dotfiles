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
const Formatter_1 = __importDefault(require("../Formatter"));
class DocumentFormattingProvider extends Provider_1.default {
    constructor(connection) {
        super(connection);
        this.handleDocumentFormattingRequest = (params) => __awaiter(this, void 0, void 0, function* () {
            const { textDocument } = params;
            return Formatter_1.default.format(textDocument).toPromise();
        });
        this.handleDocumentRangeFormattingRequest = (params) => __awaiter(this, void 0, void 0, function* () {
            const { textDocument, range } = params;
            return Formatter_1.default.format(textDocument, range).toPromise();
        });
        this.connection.onDocumentFormatting(this.handleDocumentFormattingRequest);
        this.connection.onDocumentRangeFormatting(this.handleDocumentRangeFormattingRequest);
    }
    static register(connection) {
        return new DocumentFormattingProvider(connection);
    }
}
exports.default = DocumentFormattingProvider;
//# sourceMappingURL=DocumentFormattingProvider.js.map