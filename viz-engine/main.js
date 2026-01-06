// Scientific Knowledge Mapping 3D Visualizer
// Using 3D Force Graph for precise 3D network control

class ScientificVisualizer {
    constructor() {
        this.graph = null;
        this.ws = null;
        this.currentMode = 'citation';
        this.nodes = [];
        this.links = [];
        this.nodeMap = new Map();

        this.init();
        this.connectWebSocket();
        this.setupUI();
    }

    init() {
        // Initialize 3D Force Graph
        this.graph = ForceGraph3D({
            controlType: 'orbit',
            rendererConfig: {
                antialias: true,
                alpha: true
            }
        })
        (document.getElementById('3d-graph'))

        // Configure graph appearance and behavior
        .nodeLabel(node => `${node.title || node.id}<br/>Type: ${node.type || 'unknown'}`)
        .nodeColor(node => this.getNodeColor(node))
        .nodeVal(node => Math.sqrt((node.weight || 1) + 1))
        .nodeResolution(16)
        .linkColor(link => this.getLinkColor(link))
        .linkWidth(link => Math.max(1, Math.min(5, link.weight || 1)))
        .linkDirectionalArrowLength(3)
        .linkDirectionalArrowRelPos(1)
        .linkOpacity(0.8)
        .onNodeClick(node => {
            // Center camera on clicked node
            const distance = 40;
            const distRatio = 1 + distance/Math.hypot(node.x, node.y, node.z);

            this.graph.cameraPosition(
                { x: node.x * distRatio, y: node.y * distRatio, z: node.z * distRatio },
                node,
                2000
            );

            // Send message to Emacs
            this.sendMessage('open-paper', { identifier: node.id });
        })
        .onLinkClick(link => {
            console.log('Link clicked:', link);
        });

        // Advanced controls
        this.setupAdvancedControls();
    }

    setupAdvancedControls() {
        // Custom controls for precise manipulation
        const controls = this.graph.controls();

        // Add keyboard shortcuts
        document.addEventListener('keydown', (event) => {
            switch(event.key) {
                case 'r':
                case 'R':
                    this.resetCamera();
                    break;
                case 'f':
                case 'F':
                    this.fitToScreen();
                    break;
                case 'p':
                case 'P':
                    this.pauseResumeAnimation();
                    break;
                case 'l':
                case 'L':
                    this.toggleLabels();
                    break;
            }
        });

        // Mouse wheel for zoom with finer control
        controls.addEventListener('wheel', (event) => {
            event.preventDefault();
            const zoomSpeed = 0.1;
            const delta = event.deltaY > 0 ? 1 + zoomSpeed : 1 - zoomSpeed;
            controls.dollyInOut(delta);
        });
    }

    resetCamera() {
        this.graph.cameraPosition({ x: 0, y: 0, z: 300 }, { x: 0, y: 0, z: 0 }, 2000);
    }

    fitToScreen() {
        this.graph.zoomToFit(1000, 50);
    }

    pauseResumeAnimation() {
        const isPaused = this.graph.pauseAnimation();
        this.graph.pauseAnimation(!isPaused);
        console.log(`Animation ${!isPaused ? 'paused' : 'resumed'}`);
    }

    toggleLabels() {
        const showLabels = this.graph.nodeLabel() !== null;
        this.graph.nodeLabel(showLabels ? null : node => `${node.title || node.id}`);
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
        // Convert data format
        this.nodes = this.convertNodes(data.nodes || []);
        this.links = this.convertLinks(data.edges || []);

        // Update graph
        this.graph.graphData({
            nodes: this.nodes,
            links: this.links
        });

        // Update stats
        document.getElementById('node-count').textContent = `Nodes: ${this.nodes.length}`;
        document.getElementById('edge-count').textContent = `Edges: ${this.links.length}`;

        // Auto-fit after a short delay
        setTimeout(() => this.fitToScreen(), 1000);
    }

    convertNodes(nodesData) {
        return nodesData.map(node => ({
            id: node[0], // identifier
            title: node[1] || node[0], // title or identifier
            type: node[2] || 'default', // node type
            weight: node[3] || 1, // node weight/size
            group: this.getNodeGroup(node[2] || 'default')
        }));
    }

    convertLinks(edgesData) {
        return edgesData.map(edge => ({
            source: edge[0],
            target: edge[1],
            type: edge[2] || 'default',
            weight: edge[3] || 1
        }));
    }

    getNodeGroup(nodeType) {
        switch (this.currentMode) {
            case 'citation': return nodeType === 'paper' ? 1 : 2;
            case 'concept': return nodeType === 'concept' ? 1 : 2;
            case 'author': return nodeType === 'author' ? 1 : 2;
            default: return 1;
        }
    }

    getNodeColor(node) {
        switch (this.currentMode) {
            case 'citation':
                return node.type === 'paper' ? '#4a90e2' : '#666666';
            case 'concept':
                return node.type === 'concept' ? '#50c878' : '#666666';
            case 'author':
                return node.type === 'author' ? '#ff6b6b' : '#666666';
            case 'journal':
                return node.type === 'journal' ? '#ffa500' : '#666666';
            default:
                return '#ffffff';
        }
    }

    getLinkColor(link) {
        switch (link.type) {
            case 'citation': return '#4a90e2';
            case 'parent': return '#ff6b6b';
            case 'child': return '#50c878';
            case 'sibling': return '#ffa500';
            case 'friend': return '#9b59b6';
            case 'evidence': return '#e74c3c';
            case 'method': return '#3498db';
            case 'evolution': return '#2ecc71';
            default: return '#666666';
        }
    }

    updateVariables(vars) {
        if (vars.visualizationMode) {
            this.currentMode = vars.visualizationMode;
            document.getElementById('mode-select').value = this.currentMode;
            this.refreshGraph();
        }
        if (vars.theme) {
            this.applyTheme(vars.theme);
        }
    }

    refreshGraph() {
        // Re-color nodes based on new mode
        this.graph.nodeColor(node => this.getNodeColor(node));
        this.graph.linkColor(link => this.getLinkColor(link));
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
            case 'zoom':
                this.zoomToNode(command.identifier);
                break;
        }
    }

    focusNode(identifier) {
        const node = this.nodes.find(n => n.id === identifier);
        if (node) {
            const distance = 40;
            const distRatio = 1 + distance/Math.hypot(node.x || 0, node.y || 0, node.z || 0);

            this.graph.cameraPosition(
                { x: (node.x || 0) * distRatio, y: (node.y || 0) * distRatio, z: (node.z || 0) * distRatio },
                node,
                2000
            );
        }
    }

    highlightNode(identifier) {
        // Highlight node and its connections
        this.graph.nodeColor(node => {
            if (node.id === identifier) return '#ff0000';
            const connected = this.links.some(link =>
                (link.source.id || link.source) === identifier && (link.target.id || link.target) === node.id ||
                (link.target.id || link.target) === identifier && (link.source.id || link.source) === node.id
            );
            return connected ? '#ff8800' : this.getNodeColor(node);
        });

        // Reset after 3 seconds
        setTimeout(() => this.refreshGraph(), 3000);
    }

    zoomToNode(identifier) {
        this.focusNode(identifier);
    }

    requestGraphData() {
        this.sendMessage('request-graphdata', { mode: this.currentMode });
    }

    setupUI() {
        // Mode selector
        document.getElementById('mode-select').addEventListener('change', (e) => {
            this.currentMode = e.target.value;
            this.sendMessage('set-visualization-mode', { mode: this.currentMode });
            this.refreshGraph();
            this.requestGraphData();
        });

        // Control buttons
        document.getElementById('reset-view').addEventListener('click', () => this.resetCamera());
        document.getElementById('fit-view').addEventListener('click', () => this.fitToScreen());
        document.getElementById('toggle-labels').addEventListener('click', () => this.toggleLabels());

        // Advanced controls
        document.getElementById('pause-animation').addEventListener('click', () => this.pauseResumeAnimation());

        // Search functionality
        document.getElementById('search-input').addEventListener('input', (e) => {
            const query = e.target.value.toLowerCase();
            if (query) {
                const matchingNodes = this.nodes.filter(node =>
                    node.title.toLowerCase().includes(query) || node.id.toLowerCase().includes(query)
                );

                // Highlight matching nodes
                this.graph.nodeColor(node =>
                    matchingNodes.some(mn => mn.id === node.id) ? '#00ff00' : this.getNodeColor(node)
                );
            } else {
                this.refreshGraph();
            }
        });

        // Keyboard shortcuts info
        this.setupKeyboardHints();
    }

    setupKeyboardHints() {
        const hints = document.createElement('div');
        hints.id = 'keyboard-hints';
        hints.innerHTML = `
            <div class="hint-group">
                <strong>Camera:</strong> Mouse drag • Wheel zoom • Right-click pan
            </div>
            <div class="hint-group">
                <strong>Shortcuts:</strong> R (reset) • F (fit) • P (pause) • L (labels)
            </div>
        `;
        hints.style.cssText = `
            position: absolute;
            bottom: 10px;
            right: 10px;
            background: rgba(0,0,0,0.8);
            color: #ccc;
            padding: 10px;
            border-radius: 5px;
            font-size: 12px;
            font-family: monospace;
            z-index: 1000;
        `;

        document.body.appendChild(hints);
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