"use strict";
/**
 * Forest
 */
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const tree_sitter_1 = __importDefault(require("tree-sitter"));
const tree_sitter_ruby_1 = __importDefault(require("tree-sitter-ruby"));
const rxjs_1 = require("rxjs");
const operators_1 = require("rxjs/operators");
const DocumentManager_1 = require("./DocumentManager");
var ForestEventKind;
(function (ForestEventKind) {
    ForestEventKind[ForestEventKind["OPEN"] = 0] = "OPEN";
    ForestEventKind[ForestEventKind["UPDATE"] = 1] = "UPDATE";
    ForestEventKind[ForestEventKind["DELETE"] = 2] = "DELETE";
})(ForestEventKind = exports.ForestEventKind || (exports.ForestEventKind = {}));
class Forest {
    constructor() {
        this.trees = new Map();
        this.parser = new tree_sitter_1.default();
        this.parser.setLanguage(tree_sitter_ruby_1.default);
    }
    getTree(uri) {
        return this.trees.get(uri);
    }
    createTree(uri, content) {
        const tree = this.parser.parse(content);
        this.trees.set(uri, tree);
        return tree;
    }
    // For the time being this is a full reparse for every change
    // Once we can support incremental sync we can use tree-sitter's
    // edit functionality
    updateTree(uri, content) {
        let tree = this.getTree(uri);
        if (tree) {
            tree = this.parser.parse(content);
            this.trees.set(uri, tree);
        }
        else {
            tree = this.createTree(uri, content);
        }
        return tree;
    }
    deleteTree(uri) {
        return this.trees.delete(uri);
    }
}
exports.forest = new Forest();
exports.forestStream = DocumentManager_1.documents.subject.pipe(operators_1.switchMap((event) => {
    const { kind, document } = event;
    const uri = document.uri;
    const forestEvent = {
        document,
        kind: undefined,
    };
    switch (kind) {
        case DocumentManager_1.DocumentEventKind.OPEN:
            forestEvent.tree = exports.forest.createTree(uri, document.getText());
            forestEvent.kind = ForestEventKind.OPEN;
            break;
        case DocumentManager_1.DocumentEventKind.CHANGE_CONTENT:
            forestEvent.tree = exports.forest.updateTree(uri, document.getText());
            forestEvent.kind = ForestEventKind.UPDATE;
            break;
        case DocumentManager_1.DocumentEventKind.CLOSE:
            exports.forest.deleteTree(uri);
            forestEvent.kind = ForestEventKind.DELETE;
            break;
    }
    return rxjs_1.of(forestEvent);
}));
//# sourceMappingURL=Forest.js.map