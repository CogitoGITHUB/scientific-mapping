# Scientific Mapping - Agent Guidelines

**Run tests**: `make test` or `emacs -Q -batch -L . -l ert -l tests/scientific-mapping-tests.el -f ert-run-tests-batch-and-exit`
**Run single test**: `emacs -Q -batch -L . -l ert -l tests/scientific-mapping-tests.el -f ert-run-tests-batch-and-exit -t test-name`
**Build web**: `cd scientific-mapping/viz/viz-engine && npm run build`
**Check syntax**: `emacs -Q -batch -f batch-byte-compile <file>.el`
**Install deps**: `cask install`

**Style**: kebab-case functions/vars, PascalCase modes, prefix `component-name-`. 2-space indent, 80-char max.
**Docs**: All functions need docstrings, use `;;; -*- lexical-binding: t -*-`
**Imports**: `(require 'package)` at top, group stdlib then external
**Error handling**: `condition-case` for user ops, `assert` for internal checks
**Types**: Use `defcustom` with `:type` for user options, validate inputs, safe nil access
**Module pattern**: Each component has `index.el` that requires sub-components and ends with `(provide 'component/index)`

**Project**: Emacs Lisp research tool with modular structure (core/doc/cite/viz/ai/workflow/ui/integrations).

**Fixed issues**:
- ai-integration.el: Fixed docstring quote escaping, simplified to working subset
- workflow/*.el: Fixed incorrect require statements (citation-database -> cite/index)
- keybindings.el: Removed duplicate key binding (C-c s C)
- core/mode.el: Added missing `scientific-mapping-agenda-files` variable
- doc/doc-engine.el: Added missing functions and variables
- tutorial-system.el: Replaced with minimal stub (had multiple syntax errors)
- concept-relationships/concept-relationships.el: Fixed missing closing paren in `concept-relationships--get-relations` and `concept-relationships--draw-section` functions
- concept-relationships/concept-relationships.el: Fixed all compiler warnings (docstring widths, duplicated :type, unused args, free vars, missing org-element require)

**Dual-Layer Relationship System** (COMPLETED):
- `concept-relationships/inner-page.el`: Extracts in-document relationships from org properties and internal links
- `concept-relationships/inter-page.el`: Extracts cross-document relationships, citation networks, similarity calculation
- `concept-relationships/DUAL-LAYER-DESIGN.md`: Complete design spec for inner-page/inter-page layers
- Both modules provide `get-nodes` and `get-edges` functions for 3D visualization integration

**Known remaining issues**:
- tutorial-system.el: Minimal placeholder, needs full implementation restored
- concept-relationships/concept-relationships.el: 1 minor warning (define-minor-mode docstring)
