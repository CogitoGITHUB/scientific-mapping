# Scientific Mapping - Agent Guidelines

This repository uses Org mode for all documentation. Please refer to [AGENTS.org](AGENTS.org) for the complete agent guidelines, including:

- Build/lint/test commands
- Code style guidelines
- Component architecture
- Git workflow
- And more detailed documentation

The AGENTS.org file contains the authoritative guidelines for working with this codebase.

## Testing Issues & Fixes

**Missing Dependencies**: Tests fail due to missing packages (emacsql, simple-httpd, etc.)
- **Fix**: `cask install` or manually install required packages

**Load Path Issues**: Components can't find each other in batch mode
- **Fix**: Use `-L . -L component-dir` flags when running tests

**Syntax Errors**: concept-relationships.el has parsing errors and undefined variables
- **Fix**: Define missing variables, fix function signatures, resolve parsing issues

**Interactive Testing**: For development testing
- **Command**: `M-x ert` then select test pattern
- **Single test**: `M-x ert-run-tests-interactively` on specific test

## Visualization Framework Migration

**From Three.js to Cytoscape.js**: Migrated 3D visualization to 2D network visualization
- **Why**: Cytoscape.js is specifically designed for scientific/biological networks
- **Benefits**: Better algorithms, easier implementation, more appropriate for knowledge graphs
- **Dependencies**: Updated package.json with cytoscape and layout extensions
- **Features**: Multiple layout algorithms (FCose, CoSE, Cola, Euler), search, highlighting

## Git Workflow

**Always commit and push changes**: After making any modifications to the codebase
- **Commit message format**: Use conventional commits (feat:, fix:, docs:, etc.)
- **Push immediately**: Always push commits to remote repository after committing
- **Clear messages**: Describe what changed, why, and impact in commit messages
- **Atomic commits**: Each commit should represent a single logical change