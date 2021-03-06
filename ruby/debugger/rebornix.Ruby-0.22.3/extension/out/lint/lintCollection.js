'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const Linter = require('./lib/linter');
const LintResults = require('./lib/lintResults');
class LintCollection {
    constructor(globalConfig, lintConfig, rootPath) {
        this._results = {};
        this._docLinters = {};
        this._globalConfig = globalConfig;
        this._cfg = {};
        this.cfg(lintConfig, globalConfig);
        this._rootPath = rootPath;
    }
    run(doc) {
        if (!doc)
            return;
        if (doc.languageId !== 'ruby')
            return;
        if (!this._docLinters[doc.fileName] || this._docLinters[doc.fileName].doc != doc)
            this._docLinters[doc.fileName] = new Linter(doc, this._rootPath, this._update.bind(this, doc));
        this._docLinters[doc.fileName].run(this._cfg);
    }
    _update(doc, result) {
        const linter = result.linter;
        if (!this._results[linter])
            this._results[linter] = new LintResults(linter);
        this._results[linter].updateForFile(doc.uri, result);
        return Promise.resolve();
    }
    cfg(newConfig, globalConfig) {
        let activeLinters = Object.keys(this._cfg);
        let toRemove = activeLinters.filter(l => !(l in newConfig) || !newConfig[l]);
        toRemove.forEach(l => {
            if (this._results[l]) {
                this._results[l].dispose();
                delete this._results[l];
            }
            delete this._cfg[l];
        });
        // we change the config internally, so that the config of any (awaiting) linters will be updated by reference
        for (let l in newConfig) {
            if (newConfig[l])
                this._cfg[l] = Object.assign({}, newConfig[l], globalConfig);
        }
    }
    dispose() {
        for (let l in this._results)
            this._results[l].dispose();
    }
}
exports.LintCollection = LintCollection;
//# sourceMappingURL=lintCollection.js.map