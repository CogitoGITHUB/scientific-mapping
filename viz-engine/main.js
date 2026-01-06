// Scientific Knowledge Mapping Network Visualizer
// Using Cytoscape.js for better scientific network visualization

class ScientificVisualizer {
    constructor() {
        this.cy = null;
        this.ws = null;
        this.currentMode = 'citation';
        this.layouts = {
            'cose': 'cose-bilkent',
            'fcose': 'fcose',
            'cola': 'cola',
            'euler': 'euler'
        };
        this.currentLayout = 'fcose';

        this.init();
        this.connectWebSocket();
        this.setupUI();
    }

    init() {
        // Initialize Cytoscape
        this.cy = cytoscape({
            container: document.getElementById('cy'),

            style: [
                {
                    selector: 'node',
                    style: {
                        'background-color': this.getNodeColor(),
                        'label': 'data(label)',
                        'color': '#ffffff',
                        'text-valign': 'center',
                        'text-halign': 'center',
                        'font-size': '12px',
                        'width': '40px',
                        'height': '40px',
                        'border-width': '2px',
                        'border-color': '#ffffff'
                    }
                },
                {
                    selector: 'edge',
                    style: {
                        'width': 'data(weight)',
                        'line-color': 'data(color)',
                        'target-arrow-color': 'data(color)',
                        'target-arrow-shape': 'triangle',
                        'curve-style': 'bezier',
                        'label': 'data(label)',
                        'font-size': '10px',
                        'text-background-color': '#000000',
                        'text-background-opacity': 0.7
                    }
                },
                {
                    selector: '.highlighted',
                    style: {
                        'background-color': '#ff0000',
                        'line-color': '#ff0000',
                        'target-arrow-color': '#ff0000',
                        'transition-property': 'background-color, line-color, target-arrow-color',
                        'transition-duration': '0.5s'
                    }
                }
            ],

            layout: {
                name: this.currentLayout,
                animate: true,
                animationDuration: 1000,
                fit: true,
                padding: 30
            }
        });

        // Event handlers
        this.setupEventHandlers();
    }

    setupEventHandlers() {
        this.cy.on('tap', 'node', (evt) => {
            const node = evt.target;
            this.sendMessage('open-paper', { identifier: node.data('id') });
        });

        this.cy.on('tap', 'edge', (evt) => {
            const edge = evt.target;
            console.log('Edge tapped:', edge.data());
        });
    }

    connectWebSocket() {
        this.ws = new WebSocket('ws://localhost:35903');

        this.ws.onopen = () => {
            console.log('Connected to Emacs');
            this.requestGraphData();
        };

        this.ws.onmessage = (event) => {
            const message = JSON.parse(event.data);
            this.handleMessage(message);
        };

        this.ws.onclose = () => {
            console.log('Disconnected from Emacs');
            setTimeout(() => this.connectWebSocket(), 1000);
        };
    }

    handleMessage(message) {
        switch (message.type) {
            case 'graphdata':
                this.updateGraph(message.data);
                break;
            case 'variables':
                this.updateVariables(message.data);
                break;
            case 'command':
                this.handleCommand(message.data);
                break;
        }
    }

    updateGraph(data) {
        const elements = this.convertToCytoscapeFormat(data);
        this.cy.elements().remove();
        this.cy.add(elements);
        this.runLayout();

        // Update stats
        document.getElementById('node-count').textContent = `Nodes: ${this.cy.nodes().length}`;
        document.getElementById('edge-count').textContent = `Edges: ${this.cy.edges().length}`;
    }

    convertToCytoscapeFormat(data) {
        const elements = [];

        // Add nodes
        if (data.nodes) {
            data.nodes.forEach(node => {
                elements.push({
                    data: {
                        id: node[0], // identifier
                        label: node[1] || node[0], // title or identifier
                        type: node[2] || 'default', // node type
                        weight: node[3] || 1 // node weight/size
                    }
                });
            });
        }

        // Add edges
        if (data.edges) {
            data.edges.forEach((edge, index) => {
                const edgeType = edge[2] || 'default';
                const edgeWeight = edge[3] || 1;

                elements.push({
                    data: {
                        id: `edge_${index}`,
                        source: edge[0],
                        target: edge[1],
                        color: this.getEdgeColor(edgeType),
                        weight: Math.max(1, Math.min(5, edgeWeight)),
                        label: edgeType,
                        type: edgeType
                    }
                });
            });
        }

        return elements;
    }

    getNodeColor() {
        switch (this.currentMode) {
            case 'citation': return '#4a90e2'; // Blue
            case 'concept': return '#50c878'; // Green
            case 'author': return '#ff6b6b'; // Red
            case 'journal': return '#ffa500'; // Orange
            default: return '#ffffff';
        }
    }

    getEdgeColor(edgeType) {
        switch (edgeType) {
            case 'citation': return '#4a90e2'; // Blue
            case 'parent': return '#ff6b6b';   // Red
            case 'child': return '#50c878';   // Green
            case 'sibling': return '#ffa500'; // Orange
            case 'friend': return '#9b59b6';  // Purple
            case 'evidence': return '#e74c3c'; // Dark red
            case 'method': return '#3498db';  // Light blue
            case 'evolution': return '#2ecc71'; // Light green
            default: return '#666666';        // Gray
        }
    }

    runLayout(layoutName = null) {
        const layout = layoutName || this.currentLayout;
        const layoutOptions = {
            name: layout,
            animate: true,
            animationDuration: 1000,
            fit: true,
            padding: 30
        };

        // Add layout-specific options
        switch (layout) {
            case 'fcose':
                layoutOptions.quality = 'proof';
                layoutOptions.randomize = false;
                break;
            case 'cola':
                layoutOptions.maxSimulationTime = 3000;
                break;
        }

        const layoutInstance = this.cy.layout(layoutOptions);
        layoutInstance.run();
    }

    updateVariables(vars) {
        if (vars.visualizationMode) {
            this.currentMode = vars.visualizationMode;
            document.getElementById('mode-select').value = this.currentMode;
            this.updateNodeColors();
        }
        if (vars.layout) {
            this.currentLayout = vars.layout;
            this.runLayout();
        }
        if (vars.theme) {
            this.applyTheme(vars.theme);
        }
    }

    updateNodeColors() {
        this.cy.style()
            .selector('node')
            .style('background-color', this.getNodeColor())
            .update();
    }

    applyTheme(theme) {
        if (!theme) return;

        document.body.style.background = theme.background || '#0a0a0a';
        document.body.style.color = theme.foreground || '#ffffff';

        const ui = document.getElementById('ui');
        if (ui) {
            ui.style.background = theme.background ? `rgba(${this.hexToRgb(theme.background)}, 0.9)` : 'rgba(0, 0, 0, 0.8)';
            ui.style.borderBottom = `1px solid ${theme.border || '#333'}`;
        }
    }

    hexToRgb(hex) {
        const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
        return result ? `${parseInt(result[1], 16)}, ${parseInt(result[2], 16)}, ${parseInt(result[3], 16)}` : '0, 0, 0';
    }

    handleCommand(command) {
        switch (command.commandName) {
            case 'focus':
                this.focusNode(command.identifier);
                break;
            case 'highlight':
                this.highlightNode(command.identifier);
                break;
        }
    }

    focusNode(identifier) {
        const node = this.cy.$(`node[id="${identifier}"]`);
        if (node.length > 0) {
            this.cy.animate({
                center: { eles: node },
                zoom: 2,
                duration: 1000
            });
        }
    }

    highlightNode(identifier) {
        // Remove previous highlights
        this.cy.elements().removeClass('highlighted');

        const node = this.cy.$(`node[id="${identifier}"]`);
        if (node.length > 0) {
            node.addClass('highlighted');

            // Highlight connected edges and nodes
            const connected = node.neighborhood();
            connected.addClass('highlighted');
        }
    }

    requestGraphData() {
        this.sendMessage('request-graphdata', { mode: this.currentMode });
    }

    setupUI() {
        // Mode selector
        document.getElementById('mode-select').addEventListener('change', (e) => {
            this.currentMode = e.target.value;
            this.sendMessage('set-visualization-mode', { mode: this.currentMode });
            this.updateNodeColors();
            this.requestGraphData();
        });

        // Layout selector
        document.getElementById('layout-select').addEventListener('change', (e) => {
            this.currentLayout = e.target.value;
            this.runLayout();
        });

        // Reset view
        document.getElementById('reset-view').addEventListener('click', () => {
            this.cy.fit();
            this.cy.zoom(1);
            this.cy.center();
        });

        // Toggle labels
        document.getElementById('toggle-labels').addEventListener('click', () => {
            const showLabels = document.getElementById('toggle-labels').textContent === 'Hide Labels';
            this.cy.style()
                .selector('node')
                .style('label', showLabels ? 'data(label)' : '')
                .update();
            document.getElementById('toggle-labels').textContent = showLabels ? 'Show Labels' : 'Hide Labels';
        });

        // Search functionality
        document.getElementById('search-input').addEventListener('input', (e) => {
            const query = e.target.value.toLowerCase();
            if (query) {
                const matchingNodes = this.cy.nodes().filter(node =>
                    node.data('label').toLowerCase().includes(query)
                );
                this.cy.elements().removeClass('highlighted');
                matchingNodes.addClass('highlighted');
            } else {
                this.cy.elements().removeClass('highlighted');
            }
        });
    }

    sendMessage(command, data) {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify({
                command: command,
                data: data
            }));
        }
    }
}

// Initialize when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    new ScientificVisualizer();
});