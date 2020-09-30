;;; regtab.el --- minor mode for regular tabs (in my opionion)
;;; Commentary:
;; - A tab key will insert a big space (a tab character or tab-width spaces).
;; - Shift tab will un-indent the line.
;; - Tab on a region will indent the region.
;; - Shift tab on a region will un-indent the region.
;;
;; Emacs Variables Effecting Tabs:
;;     tab-width
;;         the number of characters a tab should represent
;;
;;     indent-tabs-mode
;;         if nil, emacs will use spaces rather than \t
;;
;; Installation
;;
;; - place regtab.el in ~/.emacs.d/
;; - add the following to the bottom of your emacs config file:
;;       (add-to-list 'load-path "~/.emacs.d")
;;       (require 'regtab)
;; - invoke the minor mode with M-x regtab-mode
;;
;;; Code:

(defvar regtab-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'regtab-indent)
    (define-key map (kbd "<tab>") 'regtab-indent)
    (define-key map (kbd "<backtab>") 'regtab-deindent)
    map))

(defun regtab-tab-string ()
  "Returns a string representing a tab"
  (if indent-tabs-mode "\t" (make-string tab-width ? )))

(defun debloc () (interactive)
       (message "pos: %d, mark: %d, mark-active: %d, region-active: %d"
                (point) (mark) (if mark-active 1 0) (if (region-active-p) 1 0)))

(defun region-end-line-gated()
  "Ensures that region-end where point is at left margin doesn't tab"
  (if (= (region-end) (line-beginning-position))
      (- (region-end) 1)
    (region-end)))

(defun regtab-indent (&optional arg)
  "Indents the line or region ARG places to the right.
A place is considered `tab-width' character columns."
  (interactive)
  (if (not mark-active)
      (insert (regtab-tab-string))
    (let ((orig-point (point))
          (orig-mark (mark))
          (beg (region-beginning))
          (end (region-end-line-gated))
          (n 0)
          (deactivate-mark))
      (goto-char beg)
      (beginning-of-line)
      (set-mark end)
      (while (re-search-forward "^" (+ end (* n (length (regtab-tab-string)))) t)
        (replace-match (regtab-tab-string) nil nil)
        (setq n (+ n 1)))
      (goto-char (+ beg (length (regtab-tab-string))))
      (set-mark (+ end (* n (length (regtab-tab-string)))))
      (activate-mark)
      (if (< orig-mark orig-point) (exchange-point-and-mark)))))

(defun regtab-deindent (&optional arg)
  "Removes ARG (default 1) indent levels."
  (interactive)
  (if (not mark-active)
      (save-excursion
        (beginning-of-line)
        (re-search-forward
         (concat "^" (regtab-tab-string))
         (+ (point) (length (regtab-tab-string))) t)
        (replace-match "" nil nil))
    (let ((next-line-add-newlines t)
          (orig-point (point))
          (orig-mark (mark))
          (beg (if (< (point) (mark)) (point) (mark)))
          (end (if (< (point) (mark)) (mark) (point)))
          (line-count  (count-lines (region-beginning) (region-end-line-gated)))
          (deactivate-mark))
      (goto-char beg)
      (dotimes (i line-count)
        (beginning-of-line)
        (when (re-search-forward
               (concat "^" (regtab-tab-string))
               (+ (point) (length (regtab-tab-string))) t)
          (progn
            (setq end (- end (length (regtab-tab-string))))
            (when (eq i 0)
              (setq beg (if (<= (- beg (length (regtab-tab-string))) (line-beginning-position))
                            (line-beginning-position)
                          (- beg (length (regtab-tab-string))))))
            (replace-match "" nil nil)))
        (next-line)
        (beginning-of-line))
      (goto-char beg)
      (set-mark end)
      (activate-mark)
      (if (< orig-mark orig-point) (exchange-point-and-mark)))))

;;;###autoload
(define-minor-mode regtab-mode "Regular Tabs Minor Mode"
  :lighter " regtab" :keymap regtab-mode-map)

(provide 'regtab)
