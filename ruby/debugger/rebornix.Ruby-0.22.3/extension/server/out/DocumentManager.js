"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_languageserver_1 = require("vscode-languageserver");
const rxjs_1 = require("rxjs");
var DocumentEventKind;
(function (DocumentEventKind) {
    DocumentEventKind[DocumentEventKind["OPEN"] = 0] = "OPEN";
    DocumentEventKind[DocumentEventKind["CHANGE_CONTENT"] = 1] = "CHANGE_CONTENT";
    DocumentEventKind[DocumentEventKind["CLOSE"] = 2] = "CLOSE";
})(DocumentEventKind = exports.DocumentEventKind || (exports.DocumentEventKind = {}));
class DocumentManager {
    constructor() {
        this.documents = new vscode_languageserver_1.TextDocuments();
        this.subject = new rxjs_1.Subject();
        this.documents.onDidOpen(this.emitDocumentEvent(DocumentEventKind.OPEN));
        this.documents.onDidChangeContent(this.emitDocumentEvent(DocumentEventKind.CHANGE_CONTENT));
        this.documents.onDidClose(this.emitDocumentEvent(DocumentEventKind.CLOSE));
    }
    get(id) {
        const docId = typeof id === 'string' ? id : id.uri;
        return this.documents.get(docId);
    }
    listen(connection) {
        this.documents.listen(connection);
    }
    emitDocumentEvent(kind) {
        return ({ document }) => {
            this.subject.next({
                kind,
                document,
            });
        };
    }
}
exports.default = DocumentManager;
exports.documents = new DocumentManager();
//# sourceMappingURL=DocumentManager.js.map