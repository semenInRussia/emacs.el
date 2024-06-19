;;; my-docker.el --- My configuration of `docker' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 semenInRussia
;;; Commentary:

;; My configuration of `docker'.
;;; Code:
(require 'my-leaf)

(leaf dockerfile-mode
  :ensure (dockerfile-mode :repo "spotify/dockerfile-mode" :host github))

(provide 'my-docker)
;;; my-docker.el ends here
