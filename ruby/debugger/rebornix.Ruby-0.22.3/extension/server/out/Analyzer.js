"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const operators_1 = require("rxjs/operators");
const DocumentSymbolAnalyzer_1 = __importDefault(require("./analyzers/DocumentSymbolAnalyzer"));
const Forest_1 = require("./Forest");
const FoldingRangeAnalyzer_1 = __importDefault(require("./analyzers/FoldingRangeAnalyzer"));
class Analyzer {
    constructor(uri) {
        this.uri = uri;
        this.foldingRangeAnalyzer = new FoldingRangeAnalyzer_1.default();
        this.documentSymbolAnalyzer = new DocumentSymbolAnalyzer_1.default();
    }
    get analysis() {
        return {
            uri: this.uri,
            foldingRanges: this.foldingRangeAnalyzer.foldingRanges,
            documentSymbols: this.documentSymbolAnalyzer.symbols,
        };
    }
    analyze(tree) {
        const cursor = tree.walk();
        const walk = depth => {
            this.analyzeNode(cursor.currentNode);
            if (cursor.gotoFirstChild()) {
                do {
                    walk(depth + 1);
                } while (cursor.gotoNextSibling());
                cursor.gotoParent();
            }
        };
        walk(0);
        return this.analysis;
    }
    analyzeNode(node) {
        this.foldingRangeAnalyzer.analyze(node);
        this.documentSymbolAnalyzer.analyze(node);
    }
}
class Analyses {
    constructor() {
        this.closed = false;
        this.analyses = new Map();
    }
    next(analysis) {
        this.analyses.set(analysis.uri, analysis);
    }
    error(err) {
        console.log(err);
    }
    complete() {
        this.closed = true;
    }
    getAnalysis(uri) {
        return this.analyses.get(uri);
    }
}
exports.analyses = new Analyses();
Forest_1.forestStream
    .pipe(operators_1.map(({ kind, document, tree }) => {
    if (kind === Forest_1.ForestEventKind.DELETE) {
        return { uri: document.uri };
    }
    else {
        const analyzer = new Analyzer(document.uri);
        return analyzer.analyze(tree);
    }
}))
    .subscribe(exports.analyses);
//# sourceMappingURL=Analyzer.js.map