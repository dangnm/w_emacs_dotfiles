"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_languageserver_1 = require("vscode-languageserver");
const BaseAnalyzer_1 = __importDefault(require("./BaseAnalyzer"));
class FoldHeuristic {
    constructor(heuristic = {}) {
        this.defaultHeuristic = {
            row: 0,
            column: 0,
        };
        this.heuristic = heuristic;
    }
    get start() {
        return Object.assign({}, this.defaultHeuristic, this.heuristic.start);
    }
    get end() {
        return Object.assign({}, this.defaultHeuristic, this.heuristic.end);
    }
}
class FoldingRangeAnalyzer extends BaseAnalyzer_1.default {
    constructor() {
        super(...arguments);
        this.FOLD_NODES = new Map([
            [
                'array',
                new FoldHeuristic({
                    end: {
                        row: -1,
                    },
                }),
            ],
            ['block', new FoldHeuristic()],
            [
                'case',
                new FoldHeuristic({
                    end: {
                        row: -1,
                    },
                }),
            ],
            [
                'class',
                new FoldHeuristic({
                    end: {
                        row: -1,
                    },
                }),
            ],
            ['comment', new FoldHeuristic()],
            [
                'begin',
                new FoldHeuristic({
                    end: {
                        row: -1,
                    },
                }),
            ],
            ['do_block', new FoldHeuristic()],
            [
                'hash',
                new FoldHeuristic({
                    end: {
                        row: -1,
                    },
                }),
            ],
            [
                'heredoc_body',
                new FoldHeuristic({
                    start: {
                        row: -1,
                    },
                    end: {
                        row: -1,
                    },
                }),
            ],
            ['then', new FoldHeuristic()],
            ['else', new FoldHeuristic()],
            [
                'method',
                new FoldHeuristic({
                    end: {
                        row: -1,
                    },
                }),
            ],
            [
                'module',
                new FoldHeuristic({
                    end: {
                        row: -1,
                    },
                }),
            ],
            [
                'singleton_method',
                new FoldHeuristic({
                    end: {
                        row: -1,
                    },
                }),
            ],
        ]);
    }
    get foldingRanges() {
        return this.diagnostics;
    }
    analyze(node) {
        if (this.FOLD_NODES.has(node.type)) {
            const heuristic = this.FOLD_NODES.get(node.type);
            if (this.determineImplicitBlock(node, this.lastNodeAnalyzed) && this.diagnostics.length > 0) {
                const foldingRange = this.diagnostics[this.diagnostics.length - 1];
                foldingRange.endLine = node.endPosition.row;
                foldingRange.endCharacter = node.endPosition.column;
            }
            else {
                this.diagnostics.push({
                    startLine: node.startPosition.row + heuristic.start.row,
                    startCharacter: node.startPosition.column + heuristic.start.column,
                    endLine: node.endPosition.row + heuristic.end.row,
                    endCharacter: node.endPosition.column + heuristic.end.column,
                    kind: this.getFoldKind(node.type),
                });
            }
            this.lastNodeAnalyzed = node;
        }
    }
    getFoldKind(nodeType) {
        switch (nodeType) {
            case 'comment':
                return vscode_languageserver_1.FoldingRangeKind.Comment;
            case 'require':
                return vscode_languageserver_1.FoldingRangeKind.Imports;
            default:
                return vscode_languageserver_1.FoldingRangeKind.Region;
        }
    }
    determineImplicitBlock(node, lastNode) {
        return (node.type === 'comment' &&
            node.text[0] === '#' &&
            lastNode &&
            lastNode.type === 'comment' &&
            lastNode.text[0] === '#');
    }
}
exports.default = FoldingRangeAnalyzer;
//# sourceMappingURL=FoldingRangeAnalyzer.js.map