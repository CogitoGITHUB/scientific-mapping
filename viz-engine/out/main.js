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
        // Initialize 3D Force Graph with NO mouse controls
        this.graph = ForceGraph3D({
            controlType: false, // Disable all mouse controls
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
        .linkOpacity(0.8);

        // Initialize keyboard control state
        this.keyboardState = {
            moveForward: false,
            moveBackward: false,
            moveLeft: false,
            moveRight: false,
            moveUp: false,
            moveDown: false,
            rotateLeft: false,
            rotateRight: false,
            rotateUp: false,
            rotateDown: false,
            zoomIn: false,
            zoomOut: false
        };

        this.selectedNodeIndex = -1;
        this.cameraSpeed = 1;
        this.rotationSpeed = 0.02;
        this.zoomSpeed = 0.1;

        // Setup comprehensive keyboard controls
        this.setupKeyboardControls();
        this.startKeyboardAnimationLoop();
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

    setupKeyboardControls() {
        // Comprehensive keyboard event handlers
        document.addEventListener('keydown', (event) => {
            this.handleKeyDown(event);
        });

        document.addEventListener('keyup', (event) => {
            this.handleKeyUp(event);
        });

        // Prevent default behavior for our keys
        document.addEventListener('keydown', (event) => {
            const keys = ['w', 'a', 's', 'd', 'q', 'e', 'i', 'k', 'j', 'l', 'u', 'o',
                         'r', 'f', 'p', 'z', 'x', 'n', 'm', 'h', 'ArrowUp', 'ArrowDown',
                         'ArrowLeft', 'ArrowRight', 'PageUp', 'PageDown', 'Home', 'End'];
            if (keys.includes(event.key.toLowerCase())) {
                event.preventDefault();
            }
        });
    }

    handleKeyDown(event) {
        const key = event.key.toLowerCase();

        // Camera movement (WASD + QE for up/down)
        switch(key) {
            case 'w': this.keyboardState.moveForward = true; break;
            case 's': this.keyboardState.moveBackward = true; break;
            case 'a': this.keyboardState.moveLeft = true; break;
            case 'd': this.keyboardState.moveRight = true; break;
            case 'q': this.keyboardState.moveUp = true; break;
            case 'e': this.keyboardState.moveDown = true; break;

            // Camera rotation (IJKL)
            case 'i': this.keyboardState.rotateUp = true; break;
            case 'k': this.keyboardState.rotateDown = true; break;
            case 'j': this.keyboardState.rotateLeft = true; break;
            case 'l': this.keyboardState.rotateRight = true; break;

            // Zoom (UO)
            case 'u': this.keyboardState.zoomIn = true; break;
            case 'o': this.keyboardState.zoomOut = true; break;

            // Camera controls
            case 'r': this.resetCamera(); break;
            case 'f': this.fitToScreen(); break;
            case 'p': this.pauseResumeAnimation(); break;
            case 'z': this.toggleLabels(); break;

            // Node navigation (Arrow keys)
            case 'arrowup': this.selectNextNode(); break;
            case 'arrowdown': this.selectPreviousNode(); break;
            case 'arrowleft': this.selectPreviousConnection(); break;
            case 'arrowright': this.selectNextConnection(); break;

            // Node actions
            case 'enter': this.activateSelectedNode(); break;
            case ' ': this.highlightSelectedNode(); event.preventDefault(); break;

            // Search and navigation
            case 'x': this.clearSearch(); break;
            case 'n': this.focusOnSelectedNode(); break;
            case 'm': this.zoomToSelectedNode(); break;

            // Speed controls
            case 'pageup': this.increaseSpeed(); break;
            case 'pagedown': this.decreaseSpeed(); break;
            case 'home': this.maxSpeed(); break;
            case 'end': this.minSpeed(); break;

            // Help
            case 'h': this.showHelp(); break;
        }
    }

    handleKeyUp(event) {
        const key = event.key.toLowerCase();

        // Reset movement states
        switch(key) {
            case 'w': this.keyboardState.moveForward = false; break;
            case 's': this.keyboardState.moveBackward = false; break;
            case 'a': this.keyboardState.moveLeft = false; break;
            case 'd': this.keyboardState.moveRight = false; break;
            case 'q': this.keyboardState.moveUp = false; break;
            case 'e': this.keyboardState.moveDown = false; break;
            case 'i': this.keyboardState.rotateUp = false; break;
            case 'k': this.keyboardState.rotateDown = false; break;
            case 'j': this.keyboardState.rotateLeft = false; break;
            case 'l': this.keyboardState.rotateRight = false; break;
            case 'u': this.keyboardState.zoomIn = false; break;
            case 'o': this.keyboardState.zoomOut = false; break;
        }
    }

    startKeyboardAnimationLoop() {
        const animate = () => {
            this.updateCameraFromKeyboard();
            requestAnimationFrame(animate);
        };
        animate();
    }

    updateCameraFromKeyboard() {
        const camera = this.graph.camera();
        const speed = this.cameraSpeed;

        // Calculate movement vector
        const moveVector = { x: 0, y: 0, z: 0 };

        if (this.keyboardState.moveForward) moveVector.z -= speed;
        if (this.keyboardState.moveBackward) moveVector.z += speed;
        if (this.keyboardState.moveLeft) moveVector.x -= speed;
        if (this.keyboardState.moveRight) moveVector.x += speed;
        if (this.keyboardState.moveUp) moveVector.y += speed;
        if (this.keyboardState.moveDown) moveVector.y -= speed;

        // Apply camera rotation to movement
        const rotation = camera.rotation;
        const cosY = Math.cos(rotation.y);
        const sinY = Math.sin(rotation.y);

        const rotatedX = moveVector.x * cosY - moveVector.z * sinY;
        const rotatedZ = moveVector.x * sinY + moveVector.z * cosY;

        // Update camera position
        camera.position.x += rotatedX;
        camera.position.y += moveVector.y;
        camera.position.z += rotatedZ;

        // Handle rotation
        if (this.keyboardState.rotateLeft) camera.rotation.y += this.rotationSpeed;
        if (this.keyboardState.rotateRight) camera.rotation.y -= this.rotationSpeed;
        if (this.keyboardState.rotateUp) camera.rotation.x += this.rotationSpeed;
        if (this.keyboardState.rotateDown) camera.rotation.x -= this.rotationSpeed;

        // Handle zoom
        if (this.keyboardState.zoomIn) {
            camera.position.x *= (1 - this.zoomSpeed);
            camera.position.y *= (1 - this.zoomSpeed);
            camera.position.z *= (1 - this.zoomSpeed);
        }
        if (this.keyboardState.zoomOut) {
            camera.position.x *= (1 + this.zoomSpeed);
            camera.position.y *= (1 + this.zoomSpeed);
            camera.position.z *= (1 + this.zoomSpeed);
        }

        // Clamp rotation limits
        camera.rotation.x = Math.max(-Math.PI/2, Math.min(Math.PI/2, camera.rotation.x));
    }

    resetCamera() {
        this.graph.cameraPosition({ x: 0, y: 0, z: 300 }, { x: 0, y: 0, z: 0 }, 2000);
        console.log('Camera reset');
    }

    fitToScreen() {
        this.graph.zoomToFit(1000, 50);
        console.log('Fitted to screen');
    }

    pauseResumeAnimation() {
        const isPaused = this.graph.pauseAnimation();
        this.graph.pauseAnimation(!isPaused);
        console.log(`Animation ${!isPaused ? 'paused' : 'resumed'}`);
    }

    toggleLabels() {
        const showLabels = this.graph.nodeLabel() !== null;
        this.graph.nodeLabel(showLabels ? null : node => `${node.title || node.id}`);
        console.log(`Labels ${showLabels ? 'hidden' : 'shown'}`);
    }

    selectNextNode() {
        const nodes = this.nodes;
        if (nodes.length === 0) return;

        this.selectedNodeIndex = (this.selectedNodeIndex + 1) % nodes.length;
        this.updateNodeSelection();
    }

    selectPreviousNode() {
        const nodes = this.nodes;
        if (nodes.length === 0) return;

        this.selectedNodeIndex = this.selectedNodeIndex <= 0 ? nodes.length - 1 : this.selectedNodeIndex - 1;
        this.updateNodeSelection();
    }

    selectNextConnection() {
        if (this.selectedNodeIndex < 0) return;

        const selectedNode = this.nodes[this.selectedNodeIndex];
        const connections = this.links.filter(link =>
            link.source.id === selectedNode.id || link.target.id === selectedNode.id
        );

        if (connections.length === 0) return;

        // Find next connection in the list
        const currentConnectionIndex = connections.findIndex(conn =>
            conn.source.id === selectedNode.id || conn.target.id === selectedNode.id
        );

        const nextConnection = connections[(currentConnectionIndex + 1) % connections.length];
        const targetNodeId = nextConnection.source.id === selectedNode.id ?
                            nextConnection.target.id : nextConnection.source.id;

        const targetIndex = this.nodes.findIndex(node => node.id === targetNodeId);
        if (targetIndex >= 0) {
            this.selectedNodeIndex = targetIndex;
            this.updateNodeSelection();
        }
    }

    selectPreviousConnection() {
        if (this.selectedNodeIndex < 0) return;

        const selectedNode = this.nodes[this.selectedNodeIndex];
        const connections = this.links.filter(link =>
            link.source.id === selectedNode.id || link.target.id === selectedNode.id
        );

        if (connections.length === 0) return;

        const currentConnectionIndex = connections.findIndex(conn =>
            conn.source.id === selectedNode.id || conn.target.id === selectedNode.id
        );

        const prevConnection = connections[currentConnectionIndex <= 0 ? connections.length - 1 : currentConnectionIndex - 1];
        const targetNodeId = prevConnection.source.id === selectedNode.id ?
                            prevConnection.target.id : prevConnection.source.id;

        const targetIndex = this.nodes.findIndex(node => node.id === targetNodeId);
        if (targetIndex >= 0) {
            this.selectedNodeIndex = targetIndex;
            this.updateNodeSelection();
        }
    }

    updateNodeSelection() {
        // Reset all node colors
        this.refreshGraph();

        if (this.selectedNodeIndex >= 0 && this.selectedNodeIndex < this.nodes.length) {
            const selectedNode = this.nodes[this.selectedNodeIndex];

            // Highlight selected node
            this.graph.nodeColor(node => node.id === selectedNode.id ? '#ff0000' : this.getNodeColor(node));

            console.log(`Selected: ${selectedNode.title || selectedNode.id}`);
        }
    }

    activateSelectedNode() {
        if (this.selectedNodeIndex >= 0 && this.selectedNodeIndex < this.nodes.length) {
            const selectedNode = this.nodes[this.selectedNodeIndex];
            this.sendMessage('open-paper', { identifier: selectedNode.id });
            console.log(`Activated: ${selectedNode.title || selectedNode.id}`);
        }
    }

    highlightSelectedNode() {
        if (this.selectedNodeIndex >= 0 && this.selectedNodeIndex < this.nodes.length) {
            const selectedNode = this.nodes[this.selectedNodeIndex];

            // Highlight node and its connections
            this.graph.nodeColor(node => {
                if (node.id === selectedNode.id) return '#ff0000';
                const connected = this.links.some(link =>
                    (link.source.id || link.source) === selectedNode.id && (link.target.id || link.target) === node.id ||
                    (link.target.id || link.target) === selectedNode.id && (link.source.id || link.source) === node.id
                );
                return connected ? '#ff8800' : this.getNodeColor(node);
            });

            // Reset after 3 seconds
            setTimeout(() => this.updateNodeSelection(), 3000);
            console.log(`Highlighted connections for: ${selectedNode.title || selectedNode.id}`);
        }
    }

    focusOnSelectedNode() {
        if (this.selectedNodeIndex >= 0 && this.selectedNodeIndex < this.nodes.length) {
            const selectedNode = this.nodes[this.selectedNodeIndex];
            this.focusNode(selectedNode.id);
        }
    }

    zoomToSelectedNode() {
        if (this.selectedNodeIndex >= 0 && this.selectedNodeIndex < this.nodes.length) {
            const selectedNode = this.nodes[this.selectedNodeIndex];
            this.zoomToNode(selectedNode.id);
        }
    }

    clearSearch() {
        document.getElementById('search-input').value = '';
        this.refreshGraph();
        console.log('Search cleared');
    }

    increaseSpeed() {
        this.cameraSpeed = Math.min(5, this.cameraSpeed + 0.2);
        this.rotationSpeed = Math.min(0.1, this.rotationSpeed + 0.002);
        this.zoomSpeed = Math.min(0.5, this.zoomSpeed + 0.01);
        console.log(`Speed increased: ${this.cameraSpeed.toFixed(1)}`);
    }

    decreaseSpeed() {
        this.cameraSpeed = Math.max(0.1, this.cameraSpeed - 0.2);
        this.rotationSpeed = Math.max(0.001, this.rotationSpeed - 0.002);
        this.zoomSpeed = Math.max(0.01, this.zoomSpeed - 0.01);
        console.log(`Speed decreased: ${this.cameraSpeed.toFixed(1)}`);
    }

    maxSpeed() {
        this.cameraSpeed = 5;
        this.rotationSpeed = 0.1;
        this.zoomSpeed = 0.5;
        console.log('Maximum speed set');
    }

    minSpeed() {
        this.cameraSpeed = 0.1;
        this.rotationSpeed = 0.001;
        this.zoomSpeed = 0.01;
        console.log('Minimum speed set');
    }

    showHelp() {
        const helpText = `
KEYBOARD CONTROLS (No Mouse Required):

CAMERA MOVEMENT:
  W/S: Forward/Back     A/D: Left/Right     Q/E: Up/Down
  I/K: Rotate Up/Down   J/L: Rotate Left/Right
  U/O: Zoom In/Out

NODE NAVIGATION:
  ↑/↓: Select Next/Previous Node
  ←/→: Navigate Connections
  Enter: Activate Selected Node
  Space: Highlight Connections

CAMERA CONTROLS:
  R: Reset Camera       F: Fit to Screen
  P: Pause/Resume       Z: Toggle Labels

SEARCH & FOCUS:
  N: Focus on Selected   M: Zoom to Selected
  X: Clear Search

SPEED CONTROL:
  PageUp: Increase Speed   PageDown: Decrease Speed
  Home: Maximum Speed      End: Minimum Speed

OTHER:
  H: Show This Help

All controls work without mouse - pure keyboard navigation!`;
        console.log(helpText);
        alert(helpText);
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

        // Update status
        this.updateStatus('Ready - Press H for keyboard controls');
    }

    updateStatus(message) {
        const statusEl = document.getElementById('status');
        if (statusEl) {
            statusEl.textContent = message;
        }
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