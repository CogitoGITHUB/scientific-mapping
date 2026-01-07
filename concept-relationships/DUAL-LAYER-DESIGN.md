# Dual-Layer Relationship System Design

## Overview

This document describes a two-tier relationship system for scientific knowledge mapping:
1. **Inner-Page Relationships** (intra-document) - Within a single org file
2. **Inter-Page Relationships** (inter-document) - Between different files/documents

Both layers feed into the 3D visualization for unified exploration.

---

## Inner-Page Relationships (Intra-Document)

Relationships defined within a single org document using properties and links.

### Relationship Types

```
PROPERTY-based (org-mode properties):
  :PARENT:     - Higher-level concept this belongs to
  :CHILD:      - Sub-concepts contained within
  :RELATED:    - General association
  :SUPPORTS:   - Evidence/citation supporting this concept
  :CONTRADICTS: - Opposing viewpoint
  :BUILDS_ON:  - Prior work this extends
  :LEADS_TO:   - Future work/consequences

LINK-based (org-mode links):
  [[file:../concepts/x.org][Concept X]] - External concept reference
  [[cite:@smith2024][Smith et al.]]    - Citation reference
  [[heading:]]                          - Internal heading link
```

### Representation

```elisp
;; In-file structure example:
* Machine Learning
  :PROPERTIES:
  :PARENT: Artificial Intelligence
  :RELATED: Neural Networks, Deep Learning
  :SUPPORTS: @article-2023-ml-applications
  :END:
  
** Supervised Learning
   :PROPERTIES:
   :PARENT: Machine Learning
   :CHILD: Classification, Regression
   :BUILDS_ON: Statistics
   :END:
```

---

## Inter-Page Relationships (Inter-Document)

Relationships between different org documents in the knowledge base.

### Relationship Types

```
DOCUMENT-LEVEL:
  :CITES:          - This paper cites another
  :REFERENCES:     - Bibliography entry
  :RELATED_TO:     - Topic similarity
  :EXTENDS:        - Builds upon another document
  :CONTRADICTS:    - Contradicts another document
  :RESPONDS_TO:    - Response/reply to another
  :SUPERSEDES:     - Newer version replaces older

CONCEPT-LEVEL:
  :CONCEPT_LINK:   - Link to concept node
  :METHOD_USES:    - Uses a method from another doc
  :CASE_STUDY:     - Case study of a concept
  :APPLICATION:    - Practical application example
```

### Representation

```elisp
;; Document header properties:
:PROPERTIES:
:ID: doc-ml-2024
:TITLE: Machine Learning Survey 2024
:CITES: doc-neural-2023, doc-statistics-2022
:RELATED_TO: doc-ai-overview-2023, doc-deep-learning-2023
:EXTENDS: doc-ml-2022
:END:
```

---

## Relationship Strength & Attributes

Each relationship can have metadata:

```elisp
(:type "PARENT"
 :strength 0.9      ; 0.0-1.0 confidence
 :bidirectional t   ; applies both ways
 :source "manual"   ; "manual", "ai", "citation"
 :created "2024-01-15"
 :description "Machine learning is a subfield of AI")
```

---

## 3D Visualization Integration

### Inner-Page Layer (Micro View)
- **Within-document structure**: Show heading hierarchy
- **Local connections**: Property links to other headings
- **Dense clustering**: All relationships in one document cluster together
- **Heading nodes**: Each org heading becomes a node
- **Property edges**: Colored by relationship type

### Inter-Page Layer (Macro View)  
- **Document nodes**: Each file is a node
- **Document edges**: Citation/relationship links between files
- **Concept bridges**: Nodes that appear in multiple documents
- **Spatial grouping**: Related documents cluster geographically

### Unified View
- **Layer toggle**: View inner, outer, or combined
- **Zoom semantics**: 
  - Far: Show document clusters
  - Medium: Show concept relationships  
  - Close: Show heading-level details
- **Edge types**: Different colors for different relationship types
- **Semantic zooming**: More detail visible as you zoom in

---

## Implementation Architecture

```
concept-relationships/
├── core/
│   ├── relationship-types.el    ; Define relationship taxonomy
│   ├── inner-page.el           ; Extract in-document links
│   ├── inter-page.el           ; Extract cross-document links
│   └── strength-calculator.el  ; Compute relationship confidence
├── extractors/
│   ├── property-extractor.el   ; Parse :PROPERTIES: blocks
│   ├── link-extractor.el       ; Parse org links
│   └── citation-extractor.el   ; Parse bibliographic references
├── visualization/
│   ├── 3d-builder.el           ; Build 3D graph data structure
│   ├── layer-manager.el        ; Toggle inner/inter layers
│   └── semantic-zoom.el        ; Handle zoom-level rendering
└── index.el
```

---

## Relationship Taxonomy (Formal Definition)

```elisp
(defvar concept-relationship-types
  '((hierarchical
     (parent "is-a subtype of")
     (child "supertype of")
     (sibling "related sibling")
     (ancestor "transitive parent")
     (descendant "transitive child"))
    
    (associative
     (related "generally associated")
     (part-of "is component of")
     (has-part "contains component")
     (predecessor "came before")
     (successor "came after"))
    
    (causal
     (causes "leads to")
     (caused-by "result of")
     (enables "makes possible")
     (prevents "stops from happening"))
    
    (academic
     (cites "references as source")
     (cited-by "referenced by")
     (builds-on "extends previous work")
     (contradicts "opposes")
     (supports "provides evidence for")
     (reviews "critiques")
     (extends "continues"))))
```

---

## Benefits of Dual-Layer Design

1. **Scalability**: Inner-page doesn't overwhelm when viewing inter-page
2. **Granularity**: Choose appropriate detail level
3. **Clarity**: Different relationship types visually distinct
4. **Navigation**: Semantic zoom for exploration
5. **Traceability**: Follow knowledge flow at any level
6. **Integration**: Combines document structure with knowledge graph

---

## Example 3D View Scenarios

### Scenario 1: Literature Review
- **Macro view**: Show all papers as nodes, citation edges
- **Click on paper**: Expand to show inner-page concept map
- **Follow citation**: Jump to cited paper, see its concepts

### Scenario 2: Concept Exploration  
- **Search concept**: Find across all documents
- **Show both layers**: Documents containing concept + internal structure
- **Trace relationships**: Follow concept through multiple papers

### Scenario 3: Research Gap Analysis
- **Macro view**: Identify disconnected clusters
- **Inner view**: See what's missing in each paper
- **Gap detection**: Concepts with no supporting citations
