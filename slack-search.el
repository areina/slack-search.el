;;; slack-search.el --- Search messages in slack

;; Copyright (C) 2015 Toni Reina

;; Author: Toni Reina  <areina0@gmail.com>
;; Version: 0.1
;; Package-Requires:
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;; Code:

(require 'json)
(require 'url-http)

(defgroup slack-search nil
  "Slack search"
  :prefix "slack-search-"
  :group 'applications)

(defcustom slack-search-user-token ""
  "Your user token in slack."
  :group 'slack-search)

(defvar slack-search-api-url
  "https://slack.com/api/search.messages")

(defun slack-search-read-json-from-url (url)
  "Request URL and parse the results as a json."
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char url-http-end-of-headers)
    (json-read)))

(defun slack-search-parse-timestamp (timestamp)
  "Parse the TIMESTAMP from a unix time as a string to time."
  (seconds-to-time (string-to-number timestamp)))

(defun slack-search-format-timestamp (timestamp)
  "Format TIMESTAMP string in unix time to a more readable format (%Y-%m-%d %T)."
  (let ((ts-as-time (slack-search-parse-timestamp timestamp)))
    (format-time-string "%Y-%m-%d %T" ts-as-time)))

(defun slack-search-search-url (query)
  "Return the url for search including required params and QUERY in the querystring."
  (format "%s?token=%s&query=%s&count=300"
	  slack-search-api-url
	  slack-search-user-token
	  query))

(defun slack-search-search-messages (query)
  "Return a list of match results for a search with QUERY terms."
  (let ((response (slack-search-read-json-from-url (slack-search-search-url query))))
    (cdr (assoc 'matches (cdr (assoc 'messages response))))))

(defun slack-search-print-header-in-buffer (query)
  "Pretty print a header with QUERY info in current buffer."
  (insert (format "Results for query: '%s'\n\n" query)))

(defvar slack-search-message-format "<%s> [%s] %s\n")

(defun slack-search-format-message (message)
  "Format MESSAGE and stylize it."
  (let* ((primary? (or (cdr (assoc 'previous message)) (cdr (assoc 'next message))))
	 (face-color (if primary? "white" "darkGrey"))
	 (username (cdr (assoc 'username message)))
	 (timestamp (slack-search-format-timestamp (cdr (assoc 'ts message))))
	 (text (cdr (assoc 'text message))))

    (propertize (format slack-search-message-format
			username
			timestamp
			text)
		'font-lock-face `(:foreground ,face-color))))

(defun slack-search-print-search-results-in-buffer (results)
  "Pretty print search RESULTS in current buffer."
  (mapcar (lambda (match)
	    (insert (slack-search-format-message (cdr (assoc 'previous_2 match))))
	    (insert (slack-search-format-message (cdr (assoc 'previous match))))
	    (insert (slack-search-format-message match))
	    (insert (slack-search-format-message (cdr (assoc 'next match))))
	    (insert (slack-search-format-message (cdr (assoc 'next_2 match))))
	    (insert "\n")) results))

(defun slack-search-buffer ()
  "Return the buffer containing search results."
  (get-buffer-create "*slack-search*"))

(defun slack-search (query)
  "Search QUERY through slack API and print results in a buffer."
  (interactive "sQuery: ")
  (let ((buffer (slack-search-buffer))
	(messages (slack-search-search-messages query)))
    (with-current-buffer buffer
      (erase-buffer)
      (font-lock-mode)
      (slack-search-print-header-in-buffer query)
      (slack-search-print-search-results-in-buffer messages)
      (pop-to-buffer buffer))))

(provide 'slack-search)

;;; slack-search.el ends here
