"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const spawn_rx_1 = require("spawn-rx");
const rxjs_1 = require("rxjs");
const operators_1 = require("rxjs/operators");
class BaseLinter {
    constructor(document, config) {
        this.code = 'BaseLinter';
        this.document = document;
        this.config = config;
    }
    get cmd() {
        return this.lintConfig.command;
    }
    get args() {
        return [this.document.uri];
    }
    get lintConfig() {
        return this.config.config;
    }
    lint() {
        let { cmd, args } = this;
        if (!this.lintConfig.command && this.lintConfig.useBundler) {
            args.unshift('exec', cmd);
            cmd = 'bundle';
        }
        console.info(`Lint: executing ${cmd} ${args.join(' ')}...`);
        return spawn_rx_1.spawn(cmd, args, {
            env: this.config.env,
            cwd: this.config.executionRoot,
            stdin: rxjs_1.of(this.document.getText()),
        }).pipe(operators_1.catchError(error => {
            this.processError(error);
            return rxjs_1.empty();
        }), operators_1.reduce((acc, value) => acc + value, ''), operators_1.map((result) => this.processResults(result)));
    }
    processResults(_output) {
        return [];
    }
    isWindows() {
        return process.platform === 'win32';
    }
    processError(error) {
        switch (error.code) {
            case 'ENOENT':
                console.log(`Lint: unable to execute ${error.path} ${error.spawnargs.join(' ')} as the command could not be found`);
                break;
        }
    }
}
exports.default = BaseLinter;
//# sourceMappingURL=BaseLinter.js.map