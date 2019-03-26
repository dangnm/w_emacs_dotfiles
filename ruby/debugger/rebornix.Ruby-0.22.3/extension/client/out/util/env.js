"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const execa_1 = __importDefault(require("execa"));
const default_shell_1 = __importDefault(require("default-shell"));
const fs_1 = __importDefault(require("fs"));
const path_1 = __importDefault(require("path"));
const SHIM_DIR = path_1.default.resolve(__dirname, 'shims');
const SHIM_EXTENSION = isWindows() ? 'cmd' : 'sh';
if (!fs_1.default.existsSync(SHIM_DIR)) {
    fs_1.default.mkdirSync(SHIM_DIR);
}
function isWindows() {
    return process.platform === 'win32';
}
function getTemplate(shell) {
    let template;
    if (isWindows()) {
        template = 'SET';
    }
    else {
        template = `#!${shell} -i\nexport`;
    }
    return template;
}
function mkShim(shell, shimPath) {
    const template = getTemplate(shell);
    let result = false;
    try {
        fs_1.default.writeFileSync(shimPath, template);
        fs_1.default.chmodSync(shimPath, 0o744);
        result = true;
    }
    catch (e) {
        console.log(e);
    }
    return result;
}
function getShim() {
    const shellName = path_1.default.basename(default_shell_1.default);
    const shimPath = path_1.default.join(SHIM_DIR, `env.${shellName}.${SHIM_EXTENSION}`);
    if (!fs_1.default.existsSync(shimPath)) {
        mkShim(default_shell_1.default, shimPath);
    }
    return shimPath;
}
// Based on the dotenv parse function:
// https://github.com/motdotla/dotenv/blob/master/lib/main.js#L32
// modified to not have to deal with Buffers and to handle stuff
// like export and declare -x at the start of the line
function processExportLine(line) {
    const result = [];
    // matching "KEY' and 'VAL' in 'KEY=VAL' with support for arbitrary prefixes
    const keyValueArr = line.match(/^(?:[\w-]*\s+)*([\w.-]+)\s*=\s*(.*)?\s*$/);
    if (keyValueArr != null) {
        const key = keyValueArr[1];
        // default undefined or missing values to empty string
        let value = keyValueArr[2] || '';
        // expand newlines in quoted values
        const len = value ? value.length : 0;
        if (len > 0 && value.charAt(0) === '"' && value.charAt(len - 1) === '"') {
            value = value.replace(/\\n/gm, '\n');
        }
        // remove any surrounding quotes and extra spaces
        value = value.replace(/(^['"]|['"]$)/g, '').trim();
        result.push(key, value);
    }
    return result;
}
const RUBY_ENVIRONMENT_VARIABLES = [
    'PATH',
    'Path',
    'PATHEXT',
    'RUBY_VERSION',
    'RUBY_ROOT',
    'GEM_HOME',
    'GEM_PATH',
    'GEM_ROOT',
    'HOME',
    'RUBOCOP_OPTS',
];
function loadEnv(cwd) {
    const shim = getShim();
    const env = {};
    const { stdout, stderr } = execa_1.default.sync(shim, [], {
        cwd,
    });
    console.error(stderr);
    for (const line of stdout.split('\n')) {
        const result = processExportLine(line);
        const name = result[0];
        if (RUBY_ENVIRONMENT_VARIABLES.indexOf(name) >= 0) {
            env[name] = result[1];
        }
    }
    return env;
}
exports.loadEnv = loadEnv;
//# sourceMappingURL=env.js.map