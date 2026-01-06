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

**3D Force Graph with Universal Controls**: Keyboard + Touchscreen support for all devices
- **Why**: User required touchscreen support as must-have for laptop and mobile (hard rule)
- **Benefits**: Works on desktop, laptop touchscreens, tablets, and smartphones
- **Dependencies**: 3d-force-graph and three.js with custom control system
- **Features**: WASD/IJKL/UO keys, touch gestures (drag/rotate/pinch/tap), virtual controls on mobile, device-optimized performance

## Org Agenda Integration

**Research Task Management**: Comprehensive agenda integration for scientific workflows
- **Why**: User requested org-agenda as dependency for research project management
- **Benefits**: Schedule research tasks, track deadlines, manage literature reviews, project planning
- **Dependencies**: org-agenda (part of org package)
- **Features**: Custom agenda views, paper review scheduling, research project templates, weekly reviews, deadline tracking

## Git Workflow

**Always commit and push changes**: After making any modifications to the codebase
- **Commit message format**: Use conventional commits (feat:, fix:, docs:, etc.)
- **Push immediately**: Always push commits to remote repository after committing
- **Clear messages**: Describe what changed, why, and impact in commit messages
- **Atomic commits**: Each commit should represent a single logical change