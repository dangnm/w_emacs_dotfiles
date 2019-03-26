"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const rxjs_1 = require("rxjs");
class NullLinter {
    constructor(name) {
        this.name = name;
    }
    lint() {
        console.error(`Lint: attempted to lint with unsupported linter: ${this.name}`);
        return rxjs_1.of([]);
    }
}
exports.default = NullLinter;
//# sourceMappingURL=NullLinter.js.map