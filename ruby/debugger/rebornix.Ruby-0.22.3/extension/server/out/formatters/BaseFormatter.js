"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const spawn_rx_1 = require("spawn-rx");
const rxjs_1 = require("rxjs");
const operators_1 = require("rxjs/operators");
const diff_match_patch_1 = require("diff-match-patch");
class BaseFormatter {
    constructor(document, config) {
        this.document = document;
        this.originalText = document.getText();
        this.config = config;
        this.differ = new diff_match_patch_1.diff_match_patch();
        if (this.range) {
            this.modifyRange();
        }
    }
    get cmd() {
        return 'echo';
    }
    get args() {
        return [this.document.uri];
    }
    get useBundler() {
        return this.config.config.useBundler;
    }
    get range() {
        return this.config.range;
    }
    format() {
        let { cmd, args } = this;
        if (this.useBundler) {
            args.unshift('exec', cmd);
            cmd = 'bundle';
        }
        const formatStr = `${cmd} ${args.join(' ')}`;
        console.info(`Format: executing ${formatStr}...`);
        return spawn_rx_1.spawn(cmd, args, {
            env: this.config.env,
            cwd: this.config.executionRoot,
            stdin: rxjs_1.of(this.originalText),
        }).pipe(operators_1.catchError(error => {
            const err = this.processError(error, formatStr);
            return err ? rxjs_1.throwError(err) : rxjs_1.of('');
        }), operators_1.reduce((acc, value) => acc + value, ''), operators_1.map((result) => this.processResults(result)));
    }
    processResults(output) {
        const diffs = this.differ.diff_main(this.originalText, output);
        const edits = [];
        // VSCode wants TextEdits on the original document
        // this means position only gets moved for DIFF_EQUAL and DIFF_DELETE
        // as insert is new and doesn't have a position in the original
        let position = 0;
        for (let diff of diffs) {
            const [num, str] = diff;
            const startPos = this.document.positionAt(position);
            switch (num) {
                case diff_match_patch_1.DIFF_DELETE:
                    edits.push({
                        range: {
                            start: startPos,
                            end: this.document.positionAt(position + str.length),
                        },
                        newText: '',
                    });
                    position += str.length;
                    break;
                case diff_match_patch_1.DIFF_INSERT:
                    edits.push({
                        range: { start: startPos, end: startPos },
                        newText: str,
                    });
                    break;
                case diff_match_patch_1.DIFF_EQUAL:
                    position += str.length;
                    break;
            }
            // If we have a range we are doing a selection format. Thus,
            // only apply patches that start within the selected range
            if (this.range && num !== diff_match_patch_1.DIFF_EQUAL && !this.checkPositionInRange(startPos)) {
                edits.pop();
            }
        }
        return edits;
    }
    isWindows() {
        return process.platform === 'win32';
    }
    processError(error, formatStr) {
        let code = error.code || error.toString().match(/code: (\d+)/)[1] || null;
        let message = `\n    unable to execute ${formatStr}:\n    ${error.toString()} - ${this.messageForCode(code)}`;
        return new Error(message);
    }
    messageForCode(code) {
        switch (code) {
            case '127':
                return 'missing gem executables';
            case 'ENOENT':
                return 'command not found';
            default:
                return 'unknown error';
        }
    }
    // Modified from https://github.com/Microsoft/vscode/blob/master/src/vs/editor/common/core/range.ts#L90
    checkPositionInRange(position) {
        const { start, end } = this.range;
        if (position.line < start.line || position.line > end.line) {
            return false;
        }
        if (position.line === start.line && position.character < start.character) {
            return false;
        }
        if (position.line === end.line && position.character > end.character) {
            return false;
        }
        return true;
    }
    // If the selection range just has whitespace before it in the line,
    // extend the range to account for that whitespace
    modifyRange() {
        const { start } = this.range;
        const offset = this.document.offsetAt(start);
        const prefixLineText = this.originalText.substring(offset - start.character, offset);
        if (/^\s+$/.test(prefixLineText)) {
            this.range.start.character = 0;
        }
    }
}
exports.default = BaseFormatter;
//# sourceMappingURL=BaseFormatter.js.map