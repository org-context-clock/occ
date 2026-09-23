# AGENTS.md — occ (Org Context Clock)

GNU Emacs Lisp package: proactive, context-aware org-mode clocking (helm-based
task selection, property-based context matching). Single-package repo; no CI,
no Makefile.

**Canonical design doc: `doc/occ-design.org`.** Read it before architectural
changes, and keep it in sync when changing architecture. Other org docs in
`doc/` (design, code-flow, devel notes) are historical/stale unless they match
the code.

## Repo layout

- Root `occ-*.el` files — the package proper (`occ.el` is the entry point).
- `deps/` — vendored sibling packages, each with its own `Cask`
  (lotus-utils, lotus-helm, lotus-tree-manager, activity, org-capture+,
  org-clock-{unnamed-task, resolve-advanced, hooks, wrapper,
  daysummary, table-misc-lotus}). Do not edit `deps/` for OCC fixes.
  Other runtime deps (helm, magit, switch-buffer-functions, org-onchange,
  ert/el-mock) come from ELPA/MELPA, not `deps/`.
- `recipes/` — MELPA recipes. `recipes/occ` packages **root `*.el`**; the
  others package `deps/<pkg>/*.el`. New root-level `occ-*.el` files are
  automatically picked up by the MELPA recipe.
- `doc/` — design docs (`.org`) and demo GIF.
- `occ-skeleton.txt` — copy-paste skeleton of the `cl-defmethod`s to write
  when adding a new property.

## Commands

Package manifest is `Cask` (no Makefile):

- `cask install` — install dev dependencies
- `cask build` — build the package

Tests are `ert-deftest`s **inline in `occ-test.el`** (no `test/` directory):
`ert-occ-test`, `ert-occ-test-occ-insinuated`.

- Single test (interactive): load OCC, then `M-x ert RET <test-name> RET`
- Batch: `cask exec emacs --batch -l occ-test.el -f ert-run-tests-batch-and-exit`
- Runtime self-check of property/org consistency: `M-x occ-do-verify-objects`

Loading the package requires helm, magit, org-capture+, switch-buffer-functions
and the vendored `deps/` packages on `load-path` (runtime helpers:
`occ-add-deps-libs`, `occ-load-pkg`, `occ-reload-lib`).

## Gotchas

- `(provide 'occ)` sits at the **top** of `occ.el`, before its requires —
  deliberate cycle breaker (many files `(require 'occ)` back). Do not move it;
  circular requires among `occ-*` files are known and intentional.
- `occ-mode.el` hard-requires Spacemacs' `core-keybindings` — the package only
  loads cleanly inside Spacemacs.
- Dev deps (`ert`, `ert-x`, `el-mock`) are required unconditionally at runtime
  (`occ.el` → `occ-test.el`; also `occ-prop-base.el`).
- `Cask` `files` list is incomplete (missing required files, e.g.
  `occ-impl*.el`, `occ-filter.el`, `occ-filter-op.el`, `occ-filter-config.el`,
  `occ-normalize-ineqs.el`, `occ-capture.el`, `occ-assert.el`,
  `occ-clock-report.el`, `occ-obj-clock-method.el`, `occ-prop-org.el`,
  `occ-prop-op-misc.el`, `occ-prop-gen-misc-actions.el`); `occ-pkg.el` version
  (20190310.2156) lags the Cask version (20191027.2325). **When adding a file:
  update both Cask `files` and `occ-pkg.el`.**
- `deps/` holds vendored packages, each with its own Cask (see Repo layout;
  note `switch-buffer-functions` and `org-onchange` are **not** vendored).
  Do not edit `deps/` for OCC fixes.
- `occ-scratch-space.el` is an intentional scratchpad (body mostly wrapped in
  `(when nil ...)`) — leave as-is.

## Architecture constraints (non-obvious)

- Object model is `cl-defstruct` with `:include` inheritance + `cl-defmethod`
  dispatch — **not EIEIO `defclass`**. Dispatch happens on struct types,
  builtin types (`marker`, `buffer`, `null`, ...) and `(eql prop)` property
  symbols.
- A "property" (`currfile`, `root`, `git-branch`, `timebeing`, `status`, `key`,
  `current-clock`, ...) has no class/registry: its behavior is entirely
  `cl-defmethod`s on the `occ-obj-impl-*` / `occ-do-impl-*` generics
  specialized on `(eql prop)` (implementations in `occ-property-methods.el`;
  templates: `occ-skeleton.txt` and `doc/occ-property-methods.org`). To add a
  property, add methods.
- The supported property set per class is **derived at runtime by scanning
  cl-generic method signatures** (`occ-cl-utils.el`, which reaches into
  `cl--struct-*` / `cl-generic` internals). Don't refactor that reflection away.
- intf/impl split: `occ-obj-intf-*` (`occ-intf.el`) are thin delegators to
  `occ-obj-impl-*` (`occ-impl.el`, `occ-impl-builtin.el`,
  `occ-property-methods.el`). Extend at the impl layer.
- Org drawer keys for OCC properties are prefixed `occ-` (e.g. `CURR_FILE` →
  `OCC_CURR_FILE`); list-valued properties are space-split; values convert via
  `occ-obj-impl-to-org` / `occ-obj-impl-from-org`.
- Ranks are cached per (task, context) in `occ-ranktbl`; `occ-do-operation`
  resets affected ranks on writes — new write paths must invalidate rank caches
  the same way.
- Property priorities come from calc-solved inequality specs
  (`occ-normalize-ineqs.el`, `occ-do-add-ineq`) — weights are not hard-coded.
- Context = `occ-ctx` (buffer + file), interned in `occ-ctx-hash`; contexts are
  read-only.
- Feature discipline ("not run ahead", `doc/devel.org`): stubs signalling
  `(occ-error "Implement it.")` are intentional placeholders — don't fill them
  in casually or delete them.

## Naming conventions (from doc/devel.org)

- `occ-obj-*` = accessors/generics (nouns); `occ-do-*` = actions;
  `occ-run-*` = launchers; `occ-make-*` = fresh constructor vs `occ-build-*` =
  coerce/reuse; `*occ-*` earmuffs = global runtime state.
- Prefer polymorphic accessors over `(or ...)` parameter threading (one
  accessor covers marker + tsk + subclasses).

## File headers

Every `occ-*.el` carries: `;;; occ-xxx.el --- Title -*- lexical-binding: t; -*-`,
GPL-3 license block, `;;; Commentary:` (file role + pointer to
`doc/occ-design.org`), `;;; Code:`, `(provide 'occ-xxx)`. Keep the Commentary
accurate when changing a file's responsibilities.