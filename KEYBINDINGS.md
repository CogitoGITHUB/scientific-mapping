# Scientific Mapping - Key Bindings Reference

All commands are available under the `C-c s` prefix when `scientific-mapping-mode` is active.

## Core System Commands
| Key | Command | Description |
|-----|---------|-------------|
| `C-c s s` | `scientific-mapping-start` | Start the scientific mapping system |
| `C-c s S` | `scientific-mapping-stop` | Stop the scientific mapping system |
| `C-c s ?` | `scientific-mapping-help` | Display help for commands |

## Document Management
| Key | Command | Description |
|-----|---------|-------------|
| `C-c s i` | `scientific-mapping-import-paper` | Import paper from DOI |
| `C-c s c` | `concept-relationships-create-entry` | Create concept entry |
| `C-c s v` | `viz-engine-open` | Open 3D visualization |
| `C-c s n` | `viz-engine-set-mode` | Set visualization mode |

## Research Workflows
| Key | Command | Description |
|-----|---------|-------------|
| `C-c s r` | `scientific-mapping-literature-review` | Generate literature review |
| `C-c s b` | `scientific-mapping-backup` | Create system backup |
| `C-c s t` | `timeline-engine-open` | Open timeline view |
| `C-c s e` | `concept-tree-toggle-section` | Toggle concept tree section |

## Org Agenda Integration
| Key | Command | Description |
|-----|---------|-------------|
| `C-c s a` | `scientific-mapping-agenda` | Open research agenda |
| `C-c s p` | `scientific-mapping-schedule-paper-review` | Schedule paper review |
| `C-c s d` | `scientific-mapping-schedule-deadline` | Schedule task deadline |
| `C-c s P` | `scientific-mapping-create-research-project` | Create research project |
| `C-c s w` | `scientific-mapping-weekly-review` | Generate weekly review |
| `C-c s A` | `scientific-mapping-agenda-from-document` | Create agenda from document |

## Academic API Integrations
| Key | Command | Description |
|-----|---------|-------------|
| `C-c s X` | `academic-apis-search-arxiv` | Search arXiv database |
| `C-c s M` | `academic-apis-search-pubmed` | Search PubMed database |
| `C-c s D` | `academic-apis-resolve-doi` | Resolve DOI metadata |
| `C-c s I` | `academic-apis-import-to-scientific-mapping` | Import from academic APIs |

## Reference Manager Integration
| Key | Command | Description |
|-----|---------|-------------|
| `C-c s Z` | `reference-managers-zotero-get-collections` | Sync Zotero collections |
| `C-c s B` | `reference-managers-import-bibtex` | Import BibTeX file |
| `C-c s R` | `reference-managers-import-to-scientific-mapping` | Import from reference managers |

## AI Integration (C-c s A prefix)
| Key | Command | Description |
|-----|---------|-------------|
| `C-c s A a` | `ai-integration-analyze-document` | Analyze current document |
| `C-c s A s` | `ai-integration-summarize-document` | Summarize current document |
| `C-c s A q` | `ai-integration-ask-question` | Ask AI question |
| `C-c s A c` | `ai-integration-extract-concepts` | Extract concepts |
| `C-c s A g` | `ai-integration-detect-research-gaps` | Detect research gaps |
| `C-c s A r` | `ai-integration-research-assistance` | Get research assistance |
| `C-c s A w` | `ai-integration-writing-assistance` | Get writing assistance |
| `C-c s A i` | `ai-integration-analyze-citations` | Analyze citations |
| `C-c s A e` | `ai-integration-semantic-search` | Semantic search |
| `C-c s A b` | `ai-integration-batch-analyze` | Batch analyze directory |

## 3D Visualization Controls (Keyboard Only)

### Camera Movement
| Keys | Action |
|------|--------|
| `WASD` | Move camera forward/back/left/right |
| `QE` | Move camera up/down |
| `IJKL` | Rotate camera up/down/left/right |
| `UO` | Zoom in/out |

### Node Navigation
| Keys | Action |
|------|--------|
| `↑↓` | Select next/previous node |
| `←→` | Navigate through connections |
| `Enter` | Activate selected node |
| `Space` | Highlight node connections |

### View Controls
| Keys | Action |
|------|--------|
| `R` | Reset camera |
| `F` | Fit to screen |
| `P` | Pause/resume animation |
| `Z` | Toggle labels |

### Advanced Navigation
| Keys | Action |
|------|--------|
| `N` | Focus on selected node |
| `M` | Zoom to selected node |
| `X` | Clear search |
| `H` | Show help |

### Speed Control
| Keys | Action |
|------|--------|
| `PageUp` | Increase speed |
| `PageDown` | Decrease speed |
| `Home` | Maximum speed |
| `End` | Minimum speed |

## Touch Controls (Mobile/Laptop)

### Gestures
| Gesture | Action |
|---------|--------|
| Single finger drag | Rotate camera |
| Two finger pinch | Zoom in/out |
| Two finger rotate | Camera rotation |
| Tap on node | Select and activate |

### Virtual Controls (Mobile)
| Control | Action |
|---------|--------|
| `↑↓←→` | Move camera |
| `↻↑↓←→` | Rotate camera |
| `+/-` | Zoom in/out |
| `◀●▶` | Navigate nodes |
| `Reset/Fit` | Camera controls |

## Usage Notes

1. **Prefix Key**: All commands start with `C-c s`
2. **Mode Required**: `scientific-mapping-mode` must be active
3. **Context Awareness**: Some commands work on current buffer/document
4. **Progressive Disclosure**: Complex commands have sub-menus (like AI with `A` prefix)
5. **Keyboard First**: All functionality accessible via keyboard
6. **Touch Support**: Additional touch gestures on compatible devices

## Quick Reference

### Start Working
```
C-c s s    ; Start system
C-c s i    ; Import paper
C-c s v    ; Open visualization
C-c s a    ; Check agenda
```

### Research Workflow
```
C-c s A a  ; Analyze document
C-c s X    ; Search arXiv
C-c s p    ; Schedule review
C-c s w    ; Weekly review
```

### Get Help
```
C-c s ?    ; System help
H (in viz) ; Visualization help
```