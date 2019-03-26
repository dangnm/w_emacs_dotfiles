"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class BaseAnalyzer {
    constructor() {
        this.diagnostics = [];
    }
    analyze(node) {
        console.log(node);
    }
    flush() {
        this.diagnostics = [];
    }
}
exports.default = BaseAnalyzer;
//# sourceMappingURL=BaseAnalyzer.js.map