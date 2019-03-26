"use strict";
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) result[k] = mod[k];
    result["default"] = mod;
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = __importStar(require("vscode"));
function registerConfigurationProvider() {
    vscode.debug.registerDebugConfigurationProvider('Ruby', new RubyConfigurationProvider());
}
exports.registerConfigurationProvider = registerConfigurationProvider;
class RubyConfigurationProvider {
    provideDebugConfigurations(folder, token) {
        const names = rubyConfigurations.map((config) => config.name);
        return vscode.window.showQuickPick(names).then((selected) => {
            return [
                rubyConfigurations.find((config) => config.name === selected),
            ];
        });
    }
    resolveDebugConfiguration(folder, debugConfiguration) {
        const cwd = debugConfiguration.cwd || '${workspaceRoot}';
        return Object.assign({}, debugConfiguration, { cwd });
    }
}
const rubyConfigurations = [
    {
        name: 'Debug Local File',
        type: 'Ruby',
        request: 'launch',
        program: '${workspaceRoot}/main.rb',
    },
    {
        name: 'Listen for rdebug-ide',
        type: 'Ruby',
        request: 'attach',
        remoteHost: '127.0.0.1',
        remotePort: '1234',
        remoteWorkspaceRoot: '${workspaceRoot}',
    },
    {
        name: 'Rails server',
        type: 'Ruby',
        request: 'launch',
        program: '${workspaceRoot}/bin/rails',
        args: ['server'],
    },
    {
        name: 'RSpec - all',
        type: 'Ruby',
        request: 'launch',
        program: '${workspaceRoot}/bin/rspec',
        args: ['-I', '${workspaceRoot}'],
    },
    {
        name: 'RSpec - active spec file only',
        type: 'Ruby',
        request: 'launch',
        program: '${workspaceRoot}/bin/rspec',
        args: ['-I', '${workspaceRoot}', '${file}'],
    },
    {
        name: 'Cucumber',
        type: 'Ruby',
        request: 'launch',
        program: '${workspaceRoot}/bin/cucumber',
    },
];
//# sourceMappingURL=configuration.js.map