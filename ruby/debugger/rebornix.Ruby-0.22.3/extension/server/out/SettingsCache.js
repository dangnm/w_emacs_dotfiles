"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
class SettingsCache {
    constructor() {
        this.cache = new Map();
    }
    set(target, env) {
        const key = typeof target === 'string' ? target : target.uri;
        this.cache.set(key, env);
    }
    setAll(targets) {
        for (const target of Object.keys(targets)) {
            this.set(target, targets[target]);
        }
    }
    delete(target) {
        return this.cache.delete(target.uri);
    }
    deleteAll(targets) {
        for (const target of targets) {
            this.delete(target);
        }
    }
    get(target) {
        return __awaiter(this, void 0, void 0, function* () {
            if (!target)
                return undefined;
            const key = typeof target === 'string' ? target : target.uri;
            let settings = this.cache.get(key);
            if (!settings) {
                const result = yield this.fetcher([key]);
                settings = result.length > 0 ? result[0] : undefined;
                if (settings) {
                    this.set(key, settings);
                }
            }
            return settings;
        });
    }
    getAll(targets) {
        return __awaiter(this, void 0, void 0, function* () {
            const settings = {};
            for (const target of targets) {
                settings[target.uri] = yield this.get(target);
            }
            return settings;
        });
    }
    flush() {
        this.cache.clear();
    }
    toString() {
        return this.cache.toString();
    }
}
exports.documentConfigurationCache = new SettingsCache();
exports.workspaceRubyEnvironmentCache = new SettingsCache();
//# sourceMappingURL=SettingsCache.js.map