;;; start-viz-server.el --- Start visualization server -*- lexical-binding: t -*-

(require 'simple-httpd)

(setq httpd-root "/home/asdf/scientific-mapping/test-concepts")
(setq httpd-port 8080)

(httpd-start)

(message "Visualization server started at http://localhost:8080/")
(message "Open http://localhost:8080/visualization.html to see the 3D graph")

(sit-for 60)
