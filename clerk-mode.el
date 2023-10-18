;;; clerk-mode.el --- Evaluate Clojure buffers and present them in clerk

;; Author: Will Acton
;; Version: 0.1
;; Keywords: tools
;; Package-Requires: ((cider))

;; This file is NOT part of GNU Emacs.

;; clerk-mode.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; clerk-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cljstyle-mode.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Present Clojure buffers in clerk

;;; Code:

(require 'cider)

(defun clerk/show ()
  "Present the current buffer (if visiting a file) in the clerk viewer."
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))


(defun clerk--get-current-viewers-dynamic ()

  (read
   (nrepl-dict-get
    (nrepl-sync-request:eval
        "
 (do
 (require 'nextjournal.clerk)

 (conj
  (->>
   (nextjournal.clerk/get-default-viewers)
   (map :name)
   (remove nil?))
  :default))
"
     (cider-current-connection))
    "value")))

(defcustom clerk--current-viewers

  '(":default"
   "nextjournal.clerk.viewer/table-viewer"
   "nextjournal.clerk.viewer/markdown-viewer"
   "nextjournal.clerk.viewer/plotly-viewer"
   "nextjournal.clerk.viewer/vega-lite-viewer"
   "nextjournal.clerk.viewer/table-viewer"
   "nextjournal.clerk.viewer/row-viewer"
   "nextjournal.clerk.viewer/col-viewer"
   "nextjournal.clerk.viewer/katex-viewer"
   "nextjournal.clerk.viewer/code-viewer"
   )
  "List of viewers for selection"
  )



(defun cider-inspector-tap-current-val-with-clerk-viewer (viewer)
  "Call while in `cider-inspector` to show current value in Clerk after a viewer is selected"
  (interactive
   (list (completing-read "Choose viewer: " clerk--current-viewers
                          nil t)))

  (setq cider-inspector--current-repl (cider-current-repl))
  ;; pick some random name, which hopeuflly is never used...
  (when-let* ((ns "user")
              (var-name "cider-inspector-temp-hdhsad-hbjdbasjd842342")
              (value (cider-sync-request:inspect-def-current-val ns var-name)))

    (let ((tapped-form (make-tapped-form (concat ns "/" var-name) viewer)))
      (cider-interactive-eval tapped-form
                              nil
                              nil
                              (cider--nrepl-pr-request-map)))


    (message "%s#'%s/%s = %s" cider-eval-result-prefix ns var-name value)))

(defun clerk/serve! ()
  "Starts Clerk server and system browser"
  (interactive)
  (cider-interactive-eval "(require '[nextjournal.clerk])")
  (cider-interactive-eval "(nextjournal.clerk/serve! {:browse? true})")
  )

(defun clerk/serve-no-browser! ()
  "Starts Clerk server without browser and listens on 0.0.0.0"
  (interactive)
  (cider-interactive-eval "(require '[nextjournal.clerk])")
  (cider-interactive-eval "(nextjournal.clerk/serve! {:host \"0.0.0.0\"})")
  )


(defun clerk/open-tap-inspector ()
  "Opens Clerk's tap inspector"
  (interactive)
  (cider-interactive-eval "(require '[nextjournal.clerk])")
  (cider-interactive-eval "(nextjournal.clerk/show! 'nextjournal.clerk.tap)")
  )

(defun make-tapped-form (expr viewer)
  (concat "(clojure.core/->> "
          expr
          (if (equal "default" viewer)
              (concat " (nextjournal.clerk/with-viewer {:transform-fn identity})")
            (if (string-prefix-p ":" viewer)
                (concat " (nextjournal.clerk/with-viewer " "(keyword \"" (substring viewer 1) "\")" ")")
              (concat " (nextjournal.clerk/with-viewer " "(symbol \"" viewer "\")" ")"))
            )

          " (clojure.core/tap>))"))

(defun clerk/tap-last-sexp-with-viewer (viewer)
  "Tap the last sexp with viewer selection."
  (interactive
   (list (completing-read "Choose viewer: " clerk--current-viewers nil t)))

  (let ((tapped-form (make-tapped-form (cider-last-sexp) viewer)))
    (cider-interactive-eval tapped-form
                            nil
                            nil
                            (cider--nrepl-pr-request-map))))

(defun clerk/tap-sexp-at-point-with-viewer (viewer)
  "Tap the sexp at point with viewer selection."
  (interactive
   (list (completing-read "Choose viewer: " clerk--current-viewers nil t)))

  (let ((tapped-form (make-tapped-form (cider-sexp-at-point) viewer)))
    (cider-interactive-eval tapped-form
                            nil
                            nil
                            (cider--nrepl-pr-request-map))))

(define-minor-mode clerk-mode
  "Minor mode for presenting files in Clerk."
  :lighter " clerk"
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "M-RET") 'clerk/show)
            keymap))

(provide 'clerk-mode)

;;; clerk-mode.el ends here
