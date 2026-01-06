// Scientific Knowledge Mapping 3D Visualizer
// Inspired by org-roam-ui

class ScientificVisualizer {
    constructor() {
        this.scene = null;
        this.camera = null;
        this.renderer = null;
        this.controls = null;
        this.nodes = [];
        this.edges = [];
        this.nodeObjects = [];
        this.edgeObjects = [];
        this.labels = [];
        this.ws = null;
        this.currentMode = 'citation';
        this.showLabels = true;

        this.init();
        this.connectWebSocket();
        this.setupUI();
    }

    init() {
        // Scene setup
        this.scene = new THREE.Scene();
        this.scene.background = new THREE.Color(0x0a0a0a);

        // Camera setup
        this.camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
        this.camera.position.set(0, 0, 50);

        // Renderer setup
        this.renderer = new THREE.WebGLRenderer({ antialias: true });
        this.renderer.setSize(window.innerWidth, window.innerHeight);
        document.getElementById('scene-container').appendChild(this.renderer.domElement);

        // Lighting
        const ambientLight = new THREE.AmbientLight(0x404040, 0.6);
        this.scene.add(ambientLight);

        const directionalLight = new THREE.DirectionalLight(0xffffff, 0.8);
        directionalLight.position.set(1, 1, 1);
        this.scene.add(directionalLight);

        // Controls
        this.controls = new THREE.OrbitControls(this.camera, this.renderer.domElement);
        this.controls.enableDamping = true;
        this.controls.dampingFactor = 0.05;

        // Animation loop
        this.animate();

        // Handle window resize
        window.addEventListener('resize', () => this.onWindowResize());
    }

    animate() {
        requestAnimationFrame(() => this.animate());
        this.controls.update();
        this.renderer.render(this.scene, this.camera);
    }

    onWindowResize() {
        this.camera.aspect = window.innerWidth / window.innerHeight;
        this.camera.updateProjectionMatrix();
        this.renderer.setSize(window.innerWidth, window.innerHeight);
    }

    connectWebSocket() {
        this.ws = new WebSocket('ws://localhost:35903');

        this.ws.onopen = () => {
            console.log('Connected to Emacs');
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
        this.clearGraph();
        this.nodes = data.nodes || [];
        this.edges = data.edges || [];

        // Create nodes
        this.nodes.forEach(node => {
            this.createNode(node);
        });

        // Create edges
        this.edges.forEach(edge => {
            this.createEdge(edge);
        });

        // Update stats
        document.getElementById('node-count').textContent = `Nodes: ${this.nodes.length}`;
        document.getElementById('edge-count').textContent = `Edges: ${this.edges.length}`;

        // Auto-layout
        this.layoutGraph();
    }

    createNode(nodeData) {
        const geometry = new THREE.SphereGeometry(0.5, 16, 16);
        const material = new THREE.MeshPhongMaterial({
            color: this.getNodeColor(nodeData),
            transparent: true,
            opacity: 0.8
        });

        const node = new THREE.Mesh(geometry, material);
        node.position.set(
            (Math.random() - 0.5) * 40,
            (Math.random() - 0.5) * 40,
            (Math.random() - 0.5) * 40
        );

        node.userData = nodeData;
        this.nodeObjects.push(node);
        this.scene.add(node);

        // Create label
        if (this.showLabels) {
            this.createLabel(node, nodeData);
        }
    }

    createEdge(edgeData) {
        const sourceNode = this.nodeObjects.find(n => n.userData[0] === edgeData[0]);
        const targetNode = this.nodeObjects.find(n => n.userData[0] === edgeData[1]);

        if (sourceNode && targetNode) {
            const geometry = new THREE.BufferGeometry();
            const positions = new Float32Array([
                sourceNode.position.x, sourceNode.position.y, sourceNode.position.z,
                targetNode.position.x, targetNode.position.y, targetNode.position.z
            ]);
            geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));

            const material = new THREE.LineBasicMaterial({ color: 0x666666 });
            const edge = new THREE.Line(geometry, material);

            this.edgeObjects.push(edge);
            this.scene.add(edge);
        }
    }

    createLabel(node, nodeData) {
        // Simple text label using CSS3D
        const element = document.createElement('div');
        element.className = 'node-label';
        element.textContent = nodeData[1] || nodeData[0];
        element.style.color = 'white';
        element.style.fontSize = '12px';
        element.style.pointerEvents = 'none';

        const label = new THREE.CSS3DObject(element);
        label.position.copy(node.position);
        label.position.y += 1;

        this.labels.push(label);
        this.scene.add(label);
    }

    getNodeColor(nodeData) {
        // Color based on node type/mode
        switch (this.currentMode) {
            case 'citation':
                return 0x4a90e2; // Blue
            case 'concept':
                return 0x50c878; // Green
            case 'author':
                return 0xff6b6b; // Red
            case 'journal':
                return 0xffa500; // Orange
            default:
                return 0xffffff;
        }
    }

    layoutGraph() {
        // Simple force-directed layout simulation
        const iterations = 100;
        const repulsion = 1;
        const attraction = 0.01;

        for (let iter = 0; iter < iterations; iter++) {
            // Calculate forces
            this.nodeObjects.forEach((node, i) => {
                let forceX = 0, forceY = 0, forceZ = 0;

                // Repulsion from other nodes
                this.nodeObjects.forEach((otherNode, j) => {
                    if (i !== j) {
                        const dx = node.position.x - otherNode.position.x;
                        const dy = node.position.y - otherNode.position.y;
                        const dz = node.position.z - otherNode.position.z;
                        const distance = Math.sqrt(dx*dx + dy*dy + dz*dz) || 1;

                        const force = repulsion / (distance * distance);
                        forceX += (dx / distance) * force;
                        forceY += (dy / distance) * force;
                        forceZ += (dz / distance) * force;
                    }
                });

                // Attraction to connected nodes
                this.edges.forEach(edge => {
                    if (edge[0] === node.userData[0]) {
                        const targetNode = this.nodeObjects.find(n => n.userData[0] === edge[1]);
                        if (targetNode) {
                            const dx = targetNode.position.x - node.position.x;
                            const dy = targetNode.position.y - node.position.y;
                            const dz = targetNode.position.z - node.position.z;
                            const distance = Math.sqrt(dx*dx + dy*dy + dz*dz) || 1;

                            forceX += dx * attraction;
                            forceY += dy * attraction;
                            forceZ += dz * attraction;
                        }
                    }
                });

                // Apply force
                node.position.x += forceX * 0.01;
                node.position.y += forceY * 0.01;
                node.position.z += forceZ * 0.01;
            });
        }

        // Update edge positions
        this.updateEdges();
        this.updateLabels();
    }

    updateEdges() {
        this.edgeObjects.forEach((edge, index) => {
            const edgeData = this.edges[index];
            const sourceNode = this.nodeObjects.find(n => n.userData[0] === edgeData[0]);
            const targetNode = this.nodeObjects.find(n => n.userData[0] === edgeData[1]);

            if (sourceNode && targetNode) {
                const positions = edge.geometry.attributes.position.array;
                positions[0] = sourceNode.position.x;
                positions[1] = sourceNode.position.y;
                positions[2] = sourceNode.position.z;
                positions[3] = targetNode.position.x;
                positions[4] = targetNode.position.y;
                positions[5] = targetNode.position.z;
                edge.geometry.attributes.position.needsUpdate = true;
            }
        });
    }

    updateLabels() {
        this.labels.forEach((label, index) => {
            const node = this.nodeObjects[index];
            if (node) {
                label.position.copy(node.position);
                label.position.y += 1;
            }
        });
    }

    clearGraph() {
        this.nodeObjects.forEach(node => this.scene.remove(node));
        this.edgeObjects.forEach(edge => this.scene.remove(edge));
        this.labels.forEach(label => this.scene.remove(label));

        this.nodeObjects = [];
        this.edgeObjects = [];
        this.labels = [];
    }

    updateVariables(vars) {
        if (vars.visualizationMode) {
            this.currentMode = vars.visualizationMode;
            document.getElementById('mode-select').value = this.currentMode;
        }
    }

    handleCommand(command) {
        switch (command.commandName) {
            case 'focus':
                this.focusNode(command.identifier);
                break;
            case 'zoom':
                this.zoomToNode(command.identifier);
                break;
        }
    }

    focusNode(identifier) {
        const node = this.nodeObjects.find(n => n.userData[3] === identifier);
        if (node) {
            this.controls.target.copy(node.position);
            this.camera.position.copy(node.position);
            this.camera.position.z += 10;
        }
    }

    zoomToNode(identifier) {
        this.focusNode(identifier);
    }

    setupUI() {
        document.getElementById('mode-select').addEventListener('change', (e) => {
            this.currentMode = e.target.value;
            this.sendMessage('set-visualization-mode', { mode: this.currentMode });
        });

        document.getElementById('reset-view').addEventListener('click', () => {
            this.camera.position.set(0, 0, 50);
            this.controls.target.set(0, 0, 0);
        });

        document.getElementById('toggle-labels').addEventListener('click', () => {
            this.showLabels = !this.showLabels;
            if (this.showLabels) {
                this.nodeObjects.forEach((node, index) => {
                    this.createLabel(node, node.userData);
                });
            } else {
                this.labels.forEach(label => this.scene.remove(label));
                this.labels = [];
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