"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const assert = require("assert");
const languageConfiguration_1 = __importDefault(require("./languageConfiguration"));
describe('wordPattern', function () {
    const wordPattern = languageConfiguration_1.default.wordPattern;
    it('should not match leading colon in symbols (#257)', function () {
        const text = ':fnord';
        const matches = text.match(wordPattern);
        assert.equal(matches[0], 'fnord');
    });
    it('should not match leading colons in constants (#257)', function () {
        const text = '::Bar';
        const matches = text.match(wordPattern);
        assert.equal(matches[0], 'Bar');
    });
});
//# sourceMappingURL=languageConfiguration.test.js.map