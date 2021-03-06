/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
"use strict";
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) result[k] = mod[k];
    result["default"] = mod;
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
const assert = require("assert");
const Path = __importStar(require("path"));
const vscode_debugadapter_testsupport_1 = require("vscode-debugadapter-testsupport");
suite('Node Debug Adapter', () => {
    const DEBUG_ADAPTER = './out/debugger/main.js';
    const DATA_ROOT = Path.join(Path.join(__dirname, '../../../'), 'src/debugger/tests/data/');
    let dc;
    setup(() => {
        dc = new vscode_debugadapter_testsupport_1.DebugClient('node', DEBUG_ADAPTER, 'node');
        dc.start();
    });
    teardown(() => {
        dc.stop();
    });
    suite('basic', () => {
        test('unknown request should produce error', done => {
            dc.send('illegal_request').then(() => {
                done(new Error("does not report error on unknown request"));
            }).catch(() => {
                done();
            });
        });
    });
    suite('initialize', () => {
        test('should return supported features', () => {
            return dc.initializeRequest().then(response => {
                assert.equal(response.body.supportsConfigurationDoneRequest, true);
            });
        });
        test('should produce error for invalid \'pathFormat\'', done => {
            dc.initializeRequest({
                adapterID: 'mock',
                linesStartAt1: true,
                columnsStartAt1: true,
                pathFormat: 'url'
            }).then(response => {
                done(new Error("does not report error on invalid 'pathFormat' attribute"));
            }).catch(err => {
                // error expected
                done();
            });
        });
    });
    suite('launch', () => {
        test('should run program to the end', () => {
            const PROGRAM = Path.join(DATA_ROOT, 'basic.rb');
            return Promise.all([
                dc.configurationSequence(),
                dc.launch({ program: PROGRAM }),
                dc.waitForEvent('terminated')
            ]);
        });
        /*
        test('should stop on entry', () => {

            const PROGRAM = Path.join(DATA_ROOT, 'basic.rb');
            const ENTRY_LINE = 1;

            return Promise.all([
                dc.configurationSequence(),
                dc.launch({ program: PROGRAM, stopOnEntry: true }),
                dc.assertStoppedLocation('entry', { line: ENTRY_LINE } )
            ]);
        });
        */
    });
    suite('setBreakpoints', () => {
        // test('should stop on a breakpoint', () => {
        // 	const PROGRAM = Path.join(DATA_ROOT, 'basic.rb');
        // 	const BREAKPOINT_LINE = 2;
        // 	return Promise.all<Promise<any>>([
        // 		dc.hitBreakpoint(
        // 			{ program: PROGRAM },
        // 			{ path: PROGRAM, line: BREAKPOINT_LINE } ),
        // 		dc.assertStoppedLocation('breakpoint', { line: BREAKPOINT_LINE }),
        // 		dc.waitForEvent('stopped').then(event => {
        // 			return dc.continueRequest({
        // 				threadId: event.body.threadId
        // 			});
        // 		}),
        // 		dc.waitForEvent('terminated')]);
        // });
        test('should get correct variables on a breakpoint', done => {
            const PROGRAM = Path.join(DATA_ROOT, 'basic.rb');
            const BREAKPOINT_LINE = 3;
            return Promise.all([
                dc.hitBreakpoint({ program: PROGRAM }, { path: PROGRAM, line: BREAKPOINT_LINE }),
                dc.assertStoppedLocation('breakpoint', { line: BREAKPOINT_LINE }).then(response => {
                    return dc.scopesRequest({
                        frameId: response.body.stackFrames[0].id
                    });
                }).then(response => {
                    return dc.variablesRequest({
                        variablesReference: response.body.scopes[0].variablesReference
                    });
                }).then(response => {
                    assert.equal(response.body.variables.length, 3);
                    assert.equal(response.body.variables[0].name, "a");
                    assert.equal(response.body.variables[0].value, "10");
                    assert.equal(response.body.variables[1].name, "b");
                    assert.equal(response.body.variables[1].value, "undefined");
                    assert.equal(response.body.variables[2].name, "c");
                    assert.equal(response.body.variables[2].value, "undefined");
                    dc.disconnectRequest({});
                    done();
                })
                    .catch(err => {
                    // error expected
                    dc.disconnectRequest({});
                    done(new Error(err));
                })
            ]);
        });
    });
});
//# sourceMappingURL=adapter.test.js.map