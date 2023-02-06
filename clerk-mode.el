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

(defun clerk-show ()
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


;;  Call while in `cider-inspector` to show current value in Cler after a viewer is selected
(defun cider-inspector-tap-current-val-with-clerk-viewer (viewer)
  (interactive
   (list (completing-read "Choose viewer: " clerk--current-viewers
                          nil t)))

  (setq cider-inspector--current-repl (cider-current-repl))
  ;; pick some random name, which hopeuflly is never used...
  (when-let* ((ns "user")
              (var-name "cider-inspector-temp-hdhsad-hbjdbasjd842342")
              (value (cider-sync-request:inspect-def-current-val ns var-name)))

    (let ((tapped-form (concat
                        "(require '[nextjournal.clerk.viewer])"
                        "(clojure.core/->> "
                        (concat ns "/" var-name)
                        (if (equal ":default" viewer)
                            (concat " (nextjournal.clerk/with-viewer {:transform-fn identity})")
                          (if (string-prefix-p ":" viewer)
                              (concat " (nextjournal.clerk/with-viewer " "(keyword \"" (substring viewer 1) "\")" ")")
                            (concat " (nextjournal.clerk/with-viewer " viewer ")"))
                          )

                        " (clojure.core/tap>))")))
      (cider-interactive-eval tapped-form
                              nil
                              nil
                              (cider--nrepl-pr-request-map)))


    (message "%s#'%s/%s = %s" cider-eval-result-prefix ns var-name value)))


(defun clerk-open-tap-inspector ()
  (interactive)
  (cider-nrepl-sync-request:eval
   (concat "(require '[nextjournal.clerk :as clerk])"
           "(nextjournal.clerk/show! 'nextjournal.clerk.tap)"
           "(clerk/serve! {:browse true})"

           )))


(define-minor-mode clerk-mode
  "Minor mode for presenting files in Clerk."
  :lighter " clerk"
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "M-RET") 'clerk-show)
            keymap))

(provide 'clerk-mode)

;;; clerk-mode.el ends here
