"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// Super simple stack
class Stack {
    constructor() {
        this.stack = [];
    }
    get size() {
        return this.stack.length;
    }
    push(value) {
        this.stack.push(value);
    }
    pop() {
        return this.stack.pop();
    }
    peek() {
        return this.stack[this.size - 1];
    }
    empty() {
        return this.size === 0;
    }
}
exports.default = Stack;
//# sourceMappingURL=Stack.js.map