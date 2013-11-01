;;; go-play.el --- Paste to play.golang.org

;; Copyright (C) 2012 Dominik Honnef

;; Author: Dominik Honnef <dominikh@fork-bomb.org>
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

;;;###autoload
(defun go-play-buffer ()
  (interactive)
  (go-play-region (point-min) (point-max)))

;;;###autoload
(defun go-play-region (start end)
  (interactive "r")
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (buffer-substring-no-properties start end))
         (content-buf (url-retrieve
                       "http://play.golang.org/share"
                       (lambda (arg)
                         (cond
                          ((equal :error (car arg))
                           (signal 'go-play-error (cdr arg)))
                          (t
                           (re-search-forward "\n\n")
                           (kill-new (format "http://play.golang.org/p/%s" (buffer-substring (point) (point-max))))
                           (message "http://play.golang.org/p/%s" (buffer-substring (point) (point-max)))))))))))

(provide 'go-play)

;;; go-play.el ends here
