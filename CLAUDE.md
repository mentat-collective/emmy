# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Emmy is a Clojure(Script) mathematical computing library that serves as a modern implementation of mathematical and physics computation tools. It's designed as an alternative to MIT's scmutils system, providing symbolic and numerical computation capabilities across both Clojure and ClojureScript platforms.

## Technology Stack

- **Primary Languages**: Clojure and ClojureScript (`.cljc` files for cross-platform compatibility)
- **Build System**: Clojure CLI tools with `deps.edn`
- **Task Runner**: Babashka (`bb.edn`)
- **ClojureScript Compiler**: shadow-cljs
- **Documentation**: Nextjournal Clerk for interactive notebooks
- **Testing**: clojure.test with test.check for property-based testing
- **Linting**: clj-kondo with Emmy-specific custom linters

## Common Development Commands

### Primary Development Workflow (via Babashka)
```bash
bb repl           # Start REPL with Emmy environment loaded
bb test:clj       # Run Clojure tests
bb test:cljs      # Run ClojureScript tests
bb clerk-watch    # Start interactive documentation server
bb lint           # Lint code with clj-kondo
bb build-static   # Build static documentation
```

### Direct Clojure CLI Commands
```bash
clj -M:test:dev:repl    # REPL with full development environment
clojure -X:test:runner  # Run tests
clojure -T:build jar    # Build JAR
clojure -T:build install # Install locally
```

### ClojureScript/Node.js Commands
```bash
npm install       # Install JavaScript dependencies
npm run test      # Run ClojureScript tests
npm run watch-test # Watch tests during development
```

## Core Architecture

### Generic Operations System
Emmy implements a sophisticated generic dispatch system where mathematical operations (`+`, `-`, `*`, `/`, etc.) work across different mathematical types. The system is built around:

- `emmy.generic` - Core extensible operations
- `emmy.value/argument-kind` - Type dispatch mechanism
- Custom types participate by implementing appropriate generic methods

### Mathematical Type Hierarchy
- **Core Types**: Complex numbers, rationals, polynomials, power series, matrices, quaternions
- **Symbolic System**: Expressions with automatic simplification via `emmy.simplify`
- **Differential Objects**: Automatic differentiation support
- **Function Objects**: Mathematical functions as first-class objects

### Key Source Directories
- `src/emmy/abstract/` - Abstract mathematical concepts
- `src/emmy/calculus/` - Differential geometry, derivatives, vector fields, manifolds
- `src/emmy/mechanics/` - Classical mechanics (Lagrangian, Hamiltonian)
- `src/emmy/numerical/` - Numerical methods (integration, minimization, ODEs)
- `src/emmy/polynomial/` - Polynomial algebra
- `src/emmy/series/` - Power series and infinite series
- `src/emmy/pattern/` - Pattern matching and rule-based simplification
- `src/emmy/expression/` - Symbolic expression handling and rendering

### Cross-Platform Design
- Extensive use of `.cljc` files for Clojure/ClojureScript compatibility
- Platform-specific implementations when needed (e.g., `bigfraction.cljs`)
- Custom data readers for mathematical objects (`data_readers.cljc`)

## Testing Approach

- Tests mirror source structure in `test/emmy/`
- Comprehensive test coverage with property-based testing
- Physics examples in `test/emmy/examples/` demonstrate real-world applications
- Textbook exercises from SICM and FDG serve as integration tests

## Documentation System

Emmy uses Nextjournal Clerk for interactive documentation:
- Development server: `bb clerk-watch`
- Static build: `bb build-static`
- Source files in `dev/` directory configure Clerk notebooks
- Mathematical notation rendered as TeX
- Executable code examples throughout

## Code Quality and Linting

- Custom clj-kondo configuration in `resources/clj-kondo.exports/`
- Emmy-specific linters validate macro usage patterns
- Run linting with `bb lint`
- Exported linter configuration available for projects using Emmy

## Important Development Notes

### Generic Operations
When extending Emmy with new mathematical types, implement generic methods rather than type-specific functions. The generic system automatically handles dispatch and provides consistent behavior across the library.

### Cross-Platform Compatibility
Always use `.cljc` files when possible. Platform-specific code should be isolated and well-documented. Test both Clojure and ClojureScript implementations.

### Mathematical Rigor
Emmy maintains mathematical correctness and symbolic computation capabilities. When adding numerical methods, ensure they integrate properly with the symbolic system and provide appropriate error handling.

### Version and Release
- Version is stored in `resources/EMMY_VERSION`
- Release process uses `bb release` (publishes to Clojars)
- Documentation is deployed to GitHub Pages via `bb build-static`