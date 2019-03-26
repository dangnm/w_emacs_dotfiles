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
const Position_1 = __importDefault(require("../util/Position"));
const Forest_1 = require("../Forest");
class DocumentHighlightAnalyzer {
    static analyze(uri, position) {
        return __awaiter(this, void 0, void 0, function* () {
            const tree = Forest_1.forest.getTree(uri);
            return this.computeHighlights(tree, position);
        });
    }
    static computeHighlights(tree, position) {
        const rootNode = tree.rootNode;
        const node = rootNode.descendantForPosition(position.toTSPosition());
        let highlights = [];
        if (node.type === 'end') {
            highlights = highlights.concat(this.computeEndHighlight(node));
        }
        if (!node.isNamed && this.BEGIN_TYPES.has(node.type)) {
            highlights = highlights.concat(this.computeBeginHighlight(node));
        }
        return highlights;
    }
    static computeBeginHighlight(node) {
        const endNode = node.parent.lastChild;
        return [
            vscode_languageserver_1.DocumentHighlight.create(vscode_languageserver_1.Range.create(Position_1.default.fromTSPosition(node.startPosition).toVSPosition(), Position_1.default.fromTSPosition(node.endPosition).toVSPosition()), vscode_languageserver_1.DocumentHighlightKind.Text),
            vscode_languageserver_1.DocumentHighlight.create(vscode_languageserver_1.Range.create(Position_1.default.fromTSPosition(endNode.startPosition).toVSPosition(), Position_1.default.fromTSPosition(endNode.endPosition).toVSPosition()), vscode_languageserver_1.DocumentHighlightKind.Text),
        ];
    }
    static computeEndHighlight(node) {
        const startNode = node.parent.firstChild;
        return [
            vscode_languageserver_1.DocumentHighlight.create(vscode_languageserver_1.Range.create(Position_1.default.fromTSPosition(startNode.startPosition).toVSPosition(), Position_1.default.fromTSPosition(startNode.endPosition).toVSPosition()), vscode_languageserver_1.DocumentHighlightKind.Text),
            vscode_languageserver_1.DocumentHighlight.create(vscode_languageserver_1.Range.create(Position_1.default.fromTSPosition(node.startPosition).toVSPosition(), Position_1.default.fromTSPosition(node.endPosition).toVSPosition()), vscode_languageserver_1.DocumentHighlightKind.Text),
        ];
    }
}
DocumentHighlightAnalyzer.BEGIN_TYPES = new Set([
    'begin',
    'def',
    'if',
    'case',
    'unless',
    'do',
    'class',
    'module',
]);
exports.default = DocumentHighlightAnalyzer;
//# sourceMappingURL=DocumentHighlightAnalyzer.js.map