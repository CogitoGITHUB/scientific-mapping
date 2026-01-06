# Scientific Mapping - Agent Guidelines

## General Principles
- **Org Mode First**: Use Org mode (.org) for all documentation, notes, and structured content
- **Avoid Markdown**: Do not create .md files unless absolutely necessary (e.g., for web deployment)
- **Emacs Native**: Prefer Emacs built-in formats and tools over external alternatives
- **Consistency**: All documentation follows Org mode conventions

## Nushell Environment
**Shell Syntax**: This project uses Nushell - adjust commands accordingly
- **Sequential commands**: Use `;` instead of `&&` (e.g., `cd dir; make test`)
- **Error handling**: Use `try` instead of `||` (e.g., `try { command }`)
- **Piping**: `|` works the same as bash
- **Directory navigation**: `cd` works the same

**Nushell Integration Ideas**:
- **Data Processing**: Use Nushell pipelines to process citation databases and generate reports
- **Build Automation**: Create Nushell scripts for automated testing and deployment
- **Research Workflows**: Automate literature review processes with Nushell data manipulation
- **Export/Import**: Use Nushell to interface with external data sources and APIs

**Available Nushell Scripts**:
- `research-stats.nu`: Generate comprehensive research statistics and analytics
- `citation-export.nu`: Export citation data in CSV, JSON, or BibTeX formats
- `literature-review.nu`: Automate literature review generation with topic analysis
- See `nushell-integration.org` for complete documentation and examples

**Integration Benefits**:
- Type-safe data processing prevents research data corruption
- Pipeline-based workflows for seamless research automation
- Cross-platform compatibility with scientific mapping system
- Structured error handling for robust research data operations

## Build/Lint/Test Commands

### Testing
- **Run all tests**: `make test` or `emacs -Q -batch -l ert -l tests/scientific-mapping-tests.el -f ert-run-tests-batch-and-exit`
- **Run single test**: `emacs -Q -batch -l ert -l tests/<component>-tests.el -f ert-run-tests-batch-and-exit`
- **Interactive testing**: `M-x ert` then select test pattern

### Building
- **Build web components**: `cd viz-engine; npm run build`
- **Full build**: `make all`

### Linting
- **Check syntax**: `emacs -Q -batch -f batch-byte-compile <file>.el`
- **Package validation**: `cask install; cask build`

## Code Style Guidelines

### Org Mode Usage
- **Documentation**: Use Org mode (.org) for all documentation files
- **Configuration**: Prefer Org properties and drawers over external config files
- **Notes/Planning**: Use Org files for design documents, TODO lists, and project notes
- **Examples**: Include Org-formatted examples in docstrings when relevant
- **READMEs**: All component READMEs must be in Org format

### Emacs Lisp Conventions
- **Naming**: Use `kebab-case` for functions/variables, `PascalCase` for major modes
- **Prefixes**: Component-specific prefixes (doc-engine-, viz-engine-, etc.)
- **Documentation**: All functions need docstrings with args and return values
- **Lexical binding**: Always use `;;; -*- lexical-binding: t -*-`

### Structure
- **Components**: Each in separate directory with main `<component>.el` file
- **Tests**: Corresponding `tests/<component>-tests.el` files
- **Imports**: `(require 'package)` at top, grouped by standard library then external

### Error Handling
- **Condition-case**: Use for user-facing operations
- **Assert**: Use for internal consistency checks
- **User errors**: Clear, actionable error messages

### Types & Safety
- **Defcustom**: User options with `:type` specifications
- **Validation**: Input validation for user-provided data
- **Nil checking**: Safe access to optional properties

### Formatting
- **Indentation**: 2 spaces, follow Emacs defaults
- **Line length**: 80 characters maximum
- **Comments**: `;;` for line comments, `;;;` for section headers
- **Grouping**: Related functions grouped with section comments

### Component Architecture
- **Modular**: Each component provides single responsibility
- **Integration**: Components communicate via well-defined APIs
- **Configuration**: Customizable via defcustom variables
- **Documentation**: README.org files for all components, use Org properties for metadata
- **Org Integration**: Components should work seamlessly with Org mode features

## Key Bindings Reference
**See KEYBINDINGS.md for complete command reference and bindings**

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

## Feature Overview

### Visualization Framework Migration
**3D Force Graph with Universal Controls**: Keyboard + Touchscreen support for all devices
- **Why**: User required touchscreen support as must-have for laptop and mobile (hard rule)
- **Benefits**: Works on desktop, laptop touchscreens, tablets, and smartphones
- **Dependencies**: 3d-force-graph and three.js with custom control system
- **Features**: WASD/IJKL/UO keys, touch gestures (drag/rotate/pinch/tap), virtual controls on mobile, device-optimized performance

### Org Agenda Integration
**Research Task Management**: Comprehensive agenda integration for scientific workflows
- **Why**: User requested org-agenda as dependency for research project management
- **Benefits**: Schedule research tasks, track deadlines, manage literature reviews, project planning
- **Dependencies**: org-agenda (part of org package)
- **Features**: Custom agenda views, paper review scheduling, research project templates, weekly reviews, deadline tracking

### AI Integration
**LLM-Powered Research Assistant**: Complete AI integration for intelligent research workflows
- **Why**: Transform basic document management into AI-powered research platform
- **Benefits**: Automated analysis, intelligent insights, research acceleration
- **Providers**: Ollama (local), OpenAI, Anthropic, MCP, Agent Shell
- **Features**: Document summarization, concept extraction, question answering, writing assistance, citation analysis, research gap detection

### Academic API Integrations
**Direct Scholarly Database Access**: Seamless integration with academic ecosystems
- **Why**: Enable direct import from scholarly databases while maintaining local library focus
- **Benefits**: Stay current with latest research, automated metadata extraction, prevent duplicate work
- **APIs**: arXiv, PubMed, CrossRef DOI resolution
- **Features**: Search interfaces, structured import, Org-mode formatting, action checklists, duplicate detection
- **IMPORTANT**: Always prioritize local library management - APIs supplement, don't replace local collection

### Reference Manager Integration
**Sync with Existing Workflows**: Connect to popular reference management tools
- **Why**: Bridge gap between reference managers and research knowledge mapping
- **Benefits**: Unified workflow, no duplicate data entry, enhanced collaboration
- **Managers**: Zotero API, BibTeX import
- **Features**: Collection sync, item import, metadata preservation, batch processing

## Git Workflow

**Always commit and push changes**: After making any modifications to the codebase
- **Commit message format**: Use conventional commits (feat:, fix:, docs:, etc.)
- **Push immediately**: Always push commits to remote repository after committing
- **Clear messages**: Describe what changed, why, and impact in commit messages
- **Atomic commits**: Each commit should represent a single logical change