;;(setq gnus-default-nntp-server "news.speakeasy.net")
;;(setq gnus-default-nntp-server "news.sonic.net")
(setq gnus-default-nntp-server "news.gmane.org")

;;(setq gnus-local-organization "freelance philosophers")

(setq gnus-user-full-name nil)
(setq gnus-show-mime nil)

(setq gnus-use-generic-path "play.org")
(setq gnus-use-generic-from t) ;; strip machine name from host name...
(setq gnus-novice-user nil)
;(setq gnus-default-article-saver gnus-summary-save-in-folder)
(setq gnus-signature-file nil)

(setq gnus-article-save-directory "~/Downloads")
(setq gnus-summary-save-in-file t)

(setq gnus-subscribe-newsgroup-method ; add to bottom
      '(lambda (newsgroup)
	 (gnus-subscribe-newsgroup newsgroup nil)))

(defun rn ()
  "use gnus to read news"
  (interactive)
  ;;old:  (setq gnus-nntp-service "nntp")
  (gnus))

(defun Pnews ()
  "use gnus to post news"
  (interactive)
  ;;old:  (setq gnus-nntp-service "nntp")
  (gnus-post-news))
