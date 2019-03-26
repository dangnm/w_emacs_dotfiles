"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) result[k] = mod[k];
    result["default"] = mod;
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
const BaseAnalyzer_1 = __importDefault(require("./BaseAnalyzer"));
const util_1 = require("../util");
const RubyDocumentSymbol_1 = __importStar(require("../util/RubyDocumentSymbol"));
class DocumentSymbolAnalyzer extends BaseAnalyzer_1.default {
    constructor() {
        super();
        this.symbolStack = new util_1.Stack();
        this.nodeStack = new util_1.Stack();
    }
    get symbols() {
        return this.diagnostics;
    }
    analyze(node) {
        const symbol = RubyDocumentSymbol_1.default.build(node);
        if (symbol) {
            // empty nodeStack means we are at document root
            if (this.nodeStack.empty()) {
                this.diagnostics = this.diagnostics.concat(symbol);
            }
            else {
                const topSymbol = this.symbolStack.peek();
                topSymbol.children = topSymbol.children.concat(symbol);
            }
        }
        // Stack management
        if (RubyDocumentSymbol_1.isWrapper(node) && symbol && !Array.isArray(symbol)) {
            this.symbolStack.push(symbol);
            this.nodeStack.push(node);
        }
        else if (!this.nodeStack.empty() &&
            util_1.Position.tsPositionIsEqual(this.nodeStack.peek().endPosition, node.endPosition)) {
            this.nodeStack.pop();
            this.symbolStack.pop();
        }
    }
}
exports.default = DocumentSymbolAnalyzer;
//# sourceMappingURL=DocumentSymbolAnalyzer.js.map