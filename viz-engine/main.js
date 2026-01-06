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
                antialias: this.isMobileDevice() ? false : true, // Disable antialias on mobile for performance
                alpha: true,
                powerPreference: this.isMobileDevice() ? "low-power" : "high-performance"
            }
        })
        (document.getElementById('3d-graph'))

        // Configure graph appearance and behavior
        .nodeLabel(node => {
            if (this.isMobileDevice()) {
                return `${node.title || node.id}`; // Shorter labels on mobile
            }
            return `${node.title || node.id}<br/>Type: ${node.type || 'unknown'}`;
        })
        .nodeColor(node => this.getNodeColor(node))
        .nodeVal(node => {
            // Smaller nodes on mobile for better performance
            const baseSize = Math.sqrt((node.weight || 1) + 1);
            return this.isMobileDevice() ? Math.max(2, baseSize * 0.7) : baseSize;
        })
        .nodeResolution(this.isMobileDevice() ? 8 : 16) // Lower resolution on mobile
        .linkColor(link => this.getLinkColor(link))
        .linkWidth(link => Math.max(1, Math.min(5, link.weight || 1)))
        .linkDirectionalArrowLength(this.isMobileDevice() ? 2 : 3)
        .linkDirectionalArrowRelPos(1)
        .linkOpacity(this.isMobileDevice() ? 0.6 : 0.8); // More transparent on mobile

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

        // Touch control state
        this.touchState = {
            isDragging: false,
            lastTouchX: 0,
            lastTouchY: 0,
            touchStartX: 0,
            touchStartY: 0,
            pinchDistance: 0,
            rotationAngle: 0,
            lastPinchDistance: 0,
            lastRotationAngle: 0
        };

        // Setup comprehensive keyboard controls
        this.setupKeyboardControls();
        this.setupTouchControls();
        this.startKeyboardAnimationLoop();
        this.detectDeviceCapabilities();
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

    setupTouchControls() {
        const canvas = document.getElementById('3d-graph');

        // Touch event handlers
        canvas.addEventListener('touchstart', (event) => this.handleTouchStart(event), { passive: false });
        canvas.addEventListener('touchmove', (event) => this.handleTouchMove(event), { passive: false });
        canvas.addEventListener('touchend', (event) => this.handleTouchEnd(event), { passive: false });

        // Prevent default touch behaviors
        canvas.addEventListener('touchstart', (event) => event.preventDefault(), { passive: false });
        canvas.addEventListener('touchmove', (event) => event.preventDefault(), { passive: false });
        canvas.addEventListener('touchend', (event) => event.preventDefault(), { passive: false });

        // Add virtual controls for mobile
        if (this.isMobileDevice()) {
            this.createVirtualControls();
        }
    }

    detectDeviceCapabilities() {
        this.isTouchDevice = 'ontouchstart' in window || navigator.maxTouchPoints > 0;
        this.isMobileDevice = /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);

        console.log(`Device detection: Touch=${this.isTouchDevice}, Mobile=${this.isMobileDevice}`);
    }

    isMobileDevice() {
        return /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent) ||
               (window.innerWidth <= 768 && window.innerHeight <= 1024);
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
            this.updateCameraFromTouch();
            requestAnimationFrame(animate);
        };
        animate();
    }

    handleTouchStart(event) {
        const touches = event.touches;

        if (touches.length === 1) {
            // Single touch - prepare for drag or tap
            const touch = touches[0];
            this.touchState.touchStartX = touch.clientX;
            this.touchState.touchStartY = touch.clientY;
            this.touchState.lastTouchX = touch.clientX;
            this.touchState.lastTouchY = touch.clientY;
            this.touchState.isDragging = false;

        } else if (touches.length === 2) {
            // Two touches - prepare for pinch/rotate
            const touch1 = touches[0];
            const touch2 = touches[1];

            this.touchState.pinchDistance = this.getTouchDistance(touch1, touch2);
            this.touchState.rotationAngle = this.getTouchAngle(touch1, touch2);
            this.touchState.lastPinchDistance = this.touchState.pinchDistance;
            this.touchState.lastRotationAngle = this.touchState.rotationAngle;
        }
    }

    handleTouchMove(event) {
        const touches = event.touches;

        if (touches.length === 1) {
            // Single touch drag - camera rotation
            const touch = touches[0];
            const deltaX = touch.clientX - this.touchState.lastTouchX;
            const deltaY = touch.clientY - this.touchState.lastTouchY;

            // Mark as dragging if moved more than threshold
            if (Math.abs(deltaX) > 5 || Math.abs(deltaY) > 5) {
                this.touchState.isDragging = true;
            }

            // Apply rotation based on touch movement
            const rotationSpeed = 0.005;
            this.touchState.pendingRotationX = deltaY * rotationSpeed;
            this.touchState.pendingRotationY = deltaX * rotationSpeed;

            this.touchState.lastTouchX = touch.clientX;
            this.touchState.lastTouchY = touch.clientY;

        } else if (touches.length === 2) {
            // Two finger gestures
            const touch1 = touches[0];
            const touch2 = touches[1];

            const currentDistance = this.getTouchDistance(touch1, touch2);
            const currentAngle = this.getTouchAngle(touch1, touch2);

            // Pinch to zoom
            const pinchDelta = currentDistance - this.touchState.lastPinchDistance;
            this.touchState.pendingZoom = pinchDelta * 0.01;

            // Two finger rotate
            const angleDelta = currentAngle - this.touchState.lastRotationAngle;
            this.touchState.pendingRotationY = angleDelta * 0.01;

            this.touchState.lastPinchDistance = currentDistance;
            this.touchState.lastRotationAngle = currentAngle;
        }
    }

    handleTouchEnd(event) {
        const touches = event.changedTouches;

        // Handle tap gestures
        if (!this.touchState.isDragging && touches.length === 1) {
            const touch = touches[0];
            const deltaX = Math.abs(touch.clientX - this.touchState.touchStartX);
            const deltaY = Math.abs(touch.clientY - this.touchState.touchStartY);

            // If it was a small movement, treat as tap
            if (deltaX < 10 && deltaY < 10) {
                this.handleTap(touch.clientX, touch.clientY);
            }
        }

        // Reset touch state
        this.touchState.isDragging = false;
        this.touchState.pendingRotationX = 0;
        this.touchState.pendingRotationY = 0;
        this.touchState.pendingZoom = 0;
    }

    handleTap(clientX, clientY) {
        // Convert screen coordinates to 3D world coordinates
        const rect = document.getElementById('3d-graph').getBoundingClientRect();
        const x = ((clientX - rect.left) / rect.width) * 2 - 1;
        const y = -((clientY - rect.top) / rect.height) * 2 + 1;

        // Find nearest node to tap position
        const camera = this.graph.camera();
        const raycaster = new THREE.Raycaster();

        // Create ray from camera through tap point
        raycaster.setFromCamera({ x, y }, camera);

        // Check intersections with nodes
        const nodeObjects = this.graph.scene().children.filter(child =>
            child.userData && child.userData.id
        );

        const intersections = raycaster.intersectObjects(nodeObjects);

        if (intersections.length > 0) {
            const intersectedNode = intersections[0].object;
            const nodeId = intersectedNode.userData.id;

            // Set as selected node
            const nodeIndex = this.nodes.findIndex(node => node.id === nodeId);
            if (nodeIndex >= 0) {
                this.selectedNodeIndex = nodeIndex;
                this.updateNodeSelection();

                // Send activation message
                this.sendMessage('open-paper', { identifier: nodeId });
                console.log(`Tapped node: ${this.nodes[nodeIndex].title || nodeId}`);
            }
        }
    }

    getTouchDistance(touch1, touch2) {
        const dx = touch1.clientX - touch2.clientX;
        const dy = touch1.clientY - touch2.clientY;
        return Math.sqrt(dx * dx + dy * dy);
    }

    getTouchAngle(touch1, touch2) {
        return Math.atan2(touch2.clientY - touch1.clientY, touch2.clientX - touch1.clientX);
    }

    updateCameraFromTouch() {
        const camera = this.graph.camera();

        // Apply pending touch rotations
        if (this.touchState.pendingRotationX) {
            camera.rotation.x += this.touchState.pendingRotationX;
            camera.rotation.x = Math.max(-Math.PI/2, Math.min(Math.PI/2, camera.rotation.x));
            this.touchState.pendingRotationX *= 0.9; // Decay
        }

        if (this.touchState.pendingRotationY) {
            camera.rotation.y += this.touchState.pendingRotationY;
            this.touchState.pendingRotationY *= 0.9; // Decay
        }

        // Apply pending zoom
        if (this.touchState.pendingZoom) {
            const zoomFactor = 1 + this.touchState.pendingZoom;
            camera.position.x *= zoomFactor;
            camera.position.y *= zoomFactor;
            camera.position.z *= zoomFactor;
            this.touchState.pendingZoom *= 0.9; // Decay
        }
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

    createVirtualControls() {
        const controlsContainer = document.createElement('div');
        controlsContainer.id = 'virtual-controls';
        controlsContainer.innerHTML = `
            <div class="virtual-controls-group">
                <div class="movement-controls">
                    <button id="move-forward" class="virtual-btn">↑</button>
                    <div class="horizontal-controls">
                        <button id="move-left" class="virtual-btn">←</button>
                        <button id="move-back" class="virtual-btn">↓</button>
                        <button id="move-right" class="virtual-btn">→</button>
                    </div>
                </div>
                <div class="rotation-controls">
                    <button id="rotate-up" class="virtual-btn">↻↑</button>
                    <button id="rotate-left" class="virtual-btn">↻←</button>
                    <button id="rotate-down" class="virtual-btn">↻↓</button>
                    <button id="rotate-right" class="virtual-btn">↻→</button>
                </div>
            </div>
            <div class="virtual-controls-group">
                <button id="zoom-in" class="virtual-btn zoom-btn">+</button>
                <button id="zoom-out" class="virtual-btn zoom-btn">-</button>
                <button id="reset-camera" class="virtual-btn action-btn">Reset</button>
                <button id="fit-screen" class="virtual-btn action-btn">Fit</button>
            </div>
            <div class="virtual-controls-group">
                <button id="select-prev" class="virtual-btn nav-btn">◀</button>
                <button id="select-activate" class="virtual-btn nav-btn">●</button>
                <button id="select-next" class="virtual-btn nav-btn">▶</button>
            </div>
        `;

        controlsContainer.style.cssText = `
            position: fixed;
            bottom: 10px;
            left: 10px;
            z-index: 1000;
            display: flex;
            flex-direction: column;
            gap: 10px;
            pointer-events: auto;
        `;

        document.body.appendChild(controlsContainer);

        // Add event listeners for virtual controls
        this.setupVirtualControlEvents();
    }

    setupVirtualControlEvents() {
        // Movement controls
        document.getElementById('move-forward').addEventListener('touchstart', () => this.keyboardState.moveForward = true);
        document.getElementById('move-forward').addEventListener('touchend', () => this.keyboardState.moveForward = false);
        document.getElementById('move-back').addEventListener('touchstart', () => this.keyboardState.moveBackward = true);
        document.getElementById('move-back').addEventListener('touchend', () => this.keyboardState.moveBackward = false);
        document.getElementById('move-left').addEventListener('touchstart', () => this.keyboardState.moveLeft = true);
        document.getElementById('move-left').addEventListener('touchend', () => this.keyboardState.moveLeft = false);
        document.getElementById('move-right').addEventListener('touchstart', () => this.keyboardState.moveRight = true);
        document.getElementById('move-right').addEventListener('touchend', () => this.keyboardState.moveRight = false);

        // Rotation controls
        document.getElementById('rotate-up').addEventListener('touchstart', () => this.keyboardState.rotateUp = true);
        document.getElementById('rotate-up').addEventListener('touchend', () => this.keyboardState.rotateUp = false);
        document.getElementById('rotate-down').addEventListener('touchstart', () => this.keyboardState.rotateDown = true);
        document.getElementById('rotate-down').addEventListener('touchend', () => this.keyboardState.rotateDown = false);
        document.getElementById('rotate-left').addEventListener('touchstart', () => this.keyboardState.rotateLeft = true);
        document.getElementById('rotate-left').addEventListener('touchend', () => this.keyboardState.rotateLeft = false);
        document.getElementById('rotate-right').addEventListener('touchstart', () => this.keyboardState.rotateRight = true);
        document.getElementById('rotate-right').addEventListener('touchend', () => this.keyboardState.rotateRight = false);

        // Zoom controls
        document.getElementById('zoom-in').addEventListener('touchstart', () => this.keyboardState.zoomIn = true);
        document.getElementById('zoom-in').addEventListener('touchend', () => this.keyboardState.zoomIn = false);
        document.getElementById('zoom-out').addEventListener('touchstart', () => this.keyboardState.zoomOut = true);
        document.getElementById('zoom-out').addEventListener('touchend', () => this.keyboardState.zoomOut = false);

        // Action controls
        document.getElementById('reset-camera').addEventListener('click', () => this.resetCamera());
        document.getElementById('fit-screen').addEventListener('click', () => this.fitToScreen());

        // Node navigation
        document.getElementById('select-prev').addEventListener('click', () => this.selectPreviousNode());
        document.getElementById('select-next').addEventListener('click', () => this.selectNextNode());
        document.getElementById('select-activate').addEventListener('click', () => this.activateSelectedNode());
    }

    showHelp() {
        const isMobile = this.isMobileDevice();
        const helpText = isMobile ? `
TOUCH & VIRTUAL CONTROLS:

GESTURES:
  Single Finger Drag: Rotate camera
  Two Finger Pinch: Zoom in/out
  Two Finger Rotate: Camera rotation
  Tap Node: Select & activate

VIRTUAL CONTROLS:
  ↑↓←→: Move camera
  ↻↑↓←→: Rotate camera
  +/-: Zoom in/out
  ◀●▶: Navigate nodes
  Reset/Fit: Camera controls

KEYBOARD (if available):
  WASD: Move    IJKL: Rotate    UO: Zoom
  Arrows: Navigate    Enter: Activate
  R: Reset    F: Fit    H: Help` :

`KEYBOARD CONTROLS (No Mouse Required):

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

TOUCH SUPPORT:
  Single Drag: Rotate     Pinch: Zoom     Tap: Select
  Virtual controls available on mobile

OTHER:
  H: Show This Help

All controls work without mouse - pure keyboard navigation!`;

        console.log(helpText);
        if (isMobile) {
            alert(helpText);
        } else {
            // Create a modal for desktop
            this.showDesktopHelp(helpText);
        }
    }

    showDesktopHelp(helpText) {
        const modal = document.createElement('div');
        modal.style.cssText = `
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background: rgba(0,0,0,0.8);
            z-index: 2000;
            display: flex;
            align-items: center;
            justify-content: center;
            font-family: monospace;
            color: white;
        `;

        const content = document.createElement('div');
        content.style.cssText = `
            background: #1a1a1a;
            padding: 20px;
            border-radius: 10px;
            max-width: 600px;
            max-height: 80vh;
            overflow-y: auto;
            border: 2px solid #4a90e2;
        `;

        content.innerHTML = `
            <h2 style="color: #4a90e2; margin-top: 0;">Keyboard & Touch Controls</h2>
            <pre style="margin: 0; white-space: pre-wrap; font-size: 12px;">${helpText}</pre>
            <button onclick="this.parentElement.parentElement.remove()" style="
                margin-top: 15px;
                padding: 8px 16px;
                background: #4a90e2;
                color: white;
                border: none;
                border-radius: 4px;
                cursor: pointer;
            ">Close</button>
        `;

        modal.appendChild(content);
        document.body.appendChild(modal);
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