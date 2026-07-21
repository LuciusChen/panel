# Panel Agent Instructions

These instructions apply to the entire repository. They are adapted from the
shared Emacs Lisp coding guidelines so this project remains self-contained.

## Project-Specific Rules

- Fix the root cause and keep one implementation path. Do not add fallback
  backends, legacy schema branches, or plausible stale-data defaults.
- Do not accumulate one-use forwarding or data-shuffling helpers. A private
  helper must own a stable responsibility such as protocol decoding or request
  lifecycle cleanup.
- Keep the implementation proportional to this small package. Prefer stock
  Emacs facilities and explicit state ownership over framework-like layers.

## Elisp Best Practices

### No Side Effects on Load

Loading a file must not alter the user's active editing behavior. Activation must be explicit (user calls a command or enables a mode). Declarative package registration is acceptable at load time when it is the package boundary: mode associations, backend registrations, action registrations, cleanup hooks, and similar integrations.

### Naming

- **Public functions**: use a consistent package prefix (e.g., `pkg-`, `my-`). No double dash for public API.
- **Private/internal**: double-dash prefix (e.g., `pkg--helper`). Never call from outside the owning subsystem.
- **External private symbols**: never call another package's double-dash symbols. If the package needs behavior that only exists behind a private helper, add or request a public API in that package and depend on the version that provides it.
- **Predicates**: multi-word names end in `-p`.
- **Unused args**: prefix with `_`.

### Control Flow

- Avoid deep `let` -> `if` -> `let` chains. Favor flat, linear control flow.
- Use `if-let*`, `when-let*` for conditional binding.
- Use `pcase` / `pcase-let` for structured destructuring instead of nested `car`/`cdr`/`nth`.
- Prefer `cl-loop` over `dolist` + manual accumulators for non-trivial iteration. `cl-reduce` is acceptable for simple single-operation folds.
- Prefer idiomatic primitives over reconstructed equivalents. For example, use `vconcat` to build vectors from lists rather than `apply #'vector`, and return predicate values directly instead of wrapping them in `(not (null ...))`.

### Data Shape and Abstraction

- Prefer `let*`, `pcase-let`, alists/plists, small helpers, or table-driven mappings for short-lived context.
- Reserve `cl-defstruct` or object-style layers for stable data that crosses module or lifecycle boundaries, such as connection, result, request, or protocol state.
- A short, linear `let*` is often clearer than a one-use context object plus accessors.
- Treat helper piles as design debt. If several private functions only rename, strip, forward, or wrap data for one call path, collapse them into direct code or a table-driven mapping. If the helpers are hiding a larger ownership problem, extract the whole workflow with its state and commands instead of creating a vague `utils` module.

### Error Handling

- **`user-error`** for user-caused problems. Does NOT trigger `debug-on-error`.
- **`error`** for programmer bugs only.
- **`condition-case`** only at explicit boundaries or around genuinely recoverable non-essential operations. Do not catch internal failures just to return a plausible default.
- Do not wrap standard errors without adding semantics. Use `user-error` or `error` directly unless a wrapper adds behavior that the builtin does not provide and the docstring names that behavior.
- Error messages should state what is wrong, not what should be (e.g., "Not connected" not "Must be connected").

### State Management

- **`defvar-local`** for all per-buffer state. Set with `setq-local` in mode bodies.
- **Plain `defvar`** for global/shared state.
- **`defcustom`** for user-configurable values. Always specify `:type` precisely (`natnum`, `string`, `boolean`, `(choice ...)`) and `:group`.
- Major modes must make their state buffer-local.

### Mode Definitions

- Read-only UI buffers derive from `special-mode`.
- Editing buffers derive from the right parent (`sql-mode`, `comint-mode`, etc.).
- Register buffer-local hooks in the mode body with LOCAL=`t`.

### Rendering

- Use text properties for data-bearing annotations; overlays only for ephemeral visuals.
- Build render buffers from cached data, not by reparsing displayed text.
- Rendering should be deterministic from structured buffer-local state. Do not derive behavior from visible strings when text properties or cached data can carry the state.

### Emacs Infrastructure

- Prefer stock Emacs primitives and protocols over custom frameworks: `completing-read`, `special-mode`, standard hooks, text properties, and `text-property-search-forward` are usually better starting points than bespoke dispatch, UI, or parsing layers.
- Keep target resolution, action definition, and action presentation separate. Presentation integrations such as menus, prefix command UIs, or external action packages should not become independent business-logic systems.

### Autoloads

- Add `;;;###autoload` to user-facing commands (entry points users call via `M-x`) and user-facing minor modes.
- Do NOT autoload internal helpers, variables, or private modes.
- Internal modes that are not part of the user-facing API should use double-dash prefix (`pkg--foo-mode`).

### Completion

- Use standard `completing-read` for interactive selection unless the package has a specific reason to provide a custom reader.
- Completion-at-point functions should stay close to the Emacs protocol: compute bounds and candidates directly, return the standard completion list, and avoid a separate completion context model unless multiple real call paths share it.
- CAPFs should return quickly, use `:exclusive 'no` when they should compose with other completion sources, and avoid synchronous work that can re-enter or block the UI unless the backend explicitly supports it.
- Add CAPFs buffer-locally with `add-hook` and LOCAL=`t`.

### Dependencies

- `cl-lib` functions require `(require 'cl-lib)` — do not rely on transitive loading.
- Avoid `eval-when-compile` for runtime-needed dependencies.
- When split modules use functions or variables from sibling files, add explicit `declare-function` or `defvar` forms so byte-compilation remains honest.
- Do not use declarations as boundary patches. A new `declare-function` to a higher-level module, or to an external private symbol, is a design smell; move the interface to the owner module or expose a real public API instead.
- Compatibility shims should stay under the package's private prefix. If the
  upstream function exists, prefer a prefixed `defalias` over an unprefixed
  replacement that can confuse package tooling.
- Avoid `with-eval-after-load` in package code unless it is registering an
  optional integration at a clear package boundary.

### Quality Checks

- Every `.el` file starts with the standard file header and includes
  `-*- lexical-binding: t; -*-` on that first line.
- Every `.el` file ends by providing its corresponding feature, followed by
  `;;; file.el ends here`.
- Byte-compile with zero warnings.
- Run `checkdoc` with zero warnings.
- Run `package-lint` with zero warnings for distributable package files.
- Treat byte-compilation, `checkdoc`, and `package-lint` as mandatory pre-commit quality gates for MELPA/ELPA-style packages.
- All public `defun`, `defmacro`, `defcustom`, and `defvar` forms must have docstrings.
- Docstring first line must be a complete sentence ending with a period.
- Argument names in docstrings should be UPPERCASED.
- For split packages, run checkdoc across all distributable `*.el` files, not
  only the main package entry file.

### MELPA / Package Conventions

- First line: `;;; file.el --- Short description -*- lexical-binding: t; -*-`
  - Description must NOT contain "for Emacs" or the package name — both are redundant.
  - Keep the description under 60 characters.
- Required headers for the main package file: `;; Author:`, `;; URL:`, `;; Version:`, `;; Package-Requires:` (list all direct dependencies with minimum versions, including the declared Emacs baseline).
- In a split package, package metadata belongs in the main package file only.
  Implementation files must not carry `;; Package-Requires:` headers.
- Split implementation files still need formal license metadata, preferably
  `;; SPDX-License-Identifier:`.
- Keep required MELPA checklist attribution, such as `;; Assisted-by: ...`, in
  the main package file when tooling materially assisted the package.
- When using `package-lint` on split implementation files, configure the main
  file (for example, `package-lint-main-file`) instead of duplicating package
  metadata across files.
- Last line: `;;; file.el ends here`
- Before using a newer Emacs API, verify when the symbol was introduced (`M-x find-function`). Guard or avoid symbols above the project's declared baseline.
