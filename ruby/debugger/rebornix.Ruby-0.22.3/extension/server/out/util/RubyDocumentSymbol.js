"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_languageserver_1 = require("vscode-languageserver");
const Position_1 = __importDefault(require("./Position"));
const SYMBOLKINDS = {
    method: vscode_languageserver_1.SymbolKind.Method,
    class: vscode_languageserver_1.SymbolKind.Class,
    module: vscode_languageserver_1.SymbolKind.Module,
    assignment: vscode_languageserver_1.SymbolKind.Constant,
    method_call: vscode_languageserver_1.SymbolKind.Property,
};
const IDENTIFIER_NODES = {
    module: 'constant',
    class: 'constant',
    method: 'identifier',
    assignment: 'constant',
    method_call: 'identifier',
};
function isWrapper(node) {
    return IDENTIFIER_NODES.hasOwnProperty(node.type);
}
exports.isWrapper = isWrapper;
class RubyDocumentSymbol {
    static build(node) {
        const symbolKind = SYMBOLKINDS[node.type];
        if (!symbolKind)
            return;
        let symbol = new vscode_languageserver_1.DocumentSymbol();
        symbol.range = vscode_languageserver_1.Range.create(Position_1.default.fromTSPosition(node.startPosition).toVSPosition(), Position_1.default.fromTSPosition(node.endPosition).toVSPosition());
        symbol.kind = symbolKind;
        if (isWrapper(node)) {
            if (!node.childCount)
                return;
            const identifierNode = node.descendantsOfType(IDENTIFIER_NODES[node.type])[0];
            if (identifierNode) {
                symbol.children = [];
                symbol.name = identifierNode.text;
                if (symbol.name === 'initialize') {
                    symbol.kind = vscode_languageserver_1.SymbolKind.Constructor;
                }
                // detect attr_ method calls
                if (symbol.name.indexOf('attr_') === 0) {
                    const argumentList = node.descendantsOfType('argument_list')[0];
                    const symbols = [];
                    for (const child of argumentList.children) {
                        if (!child.isNamed)
                            continue;
                        const newSymbol = Object.assign({}, symbol);
                        newSymbol.name = child.text[0] === ':' ? child.text.substring(1) : child.text;
                        newSymbol.selectionRange = vscode_languageserver_1.Range.create(Position_1.default.fromTSPosition(child.startPosition).toVSPosition(), Position_1.default.fromTSPosition(child.endPosition).toVSPosition());
                        symbols.push(newSymbol);
                    }
                    return symbols;
                }
                else if (node.type !== 'method_call') {
                    symbol.selectionRange = vscode_languageserver_1.Range.create(Position_1.default.fromTSPosition(identifierNode.startPosition).toVSPosition(), Position_1.default.fromTSPosition(identifierNode.endPosition).toVSPosition());
                }
                else {
                    return;
                }
            }
            else {
                return;
            }
        }
        else {
            symbol.selectionRange = symbol.range;
            symbol.name = node.text;
        }
        return symbol;
    }
}
exports.default = RubyDocumentSymbol;
//# sourceMappingURL=RubyDocumentSymbol.js.map