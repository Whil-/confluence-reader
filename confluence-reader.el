;;; confluence-reader.el --- Use the Confluence API to search and read pages -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sebastián Monía
;;
;; Author: Sebastián Monía <code@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/confluence-reader.el
;; Package-Requires: ((emacs "27.1"))
;; Version: 1.0
;; Keywords: convenience tools

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; You cannot navigate Confluence using EWW, because it requires JS.
;; While I appreciate that you can create super-duper dynamic pages using
;; Confluence, in _most_ cases, the pages are just text with some images
;; intersped, or a table. And if you are ok with a read-only view (which you,
;; dear reader, probably are) then, why not consume the pages from our beloved
;; kitchen sink editor?
;; Entry points are `confluence-search', `confluence-page-from-url' and
;; `confluence-page-by-id'.
;; You need to add a token to an auth-source-enabled place, using
;; `confluence-host' as the key. And then you are good to go!

;;; Code:

(require 'auth-source)
(require 'bookmark)
(require 'cl-lib)
(require 'shr)

(defgroup confluence-reader nil
  "Search and read Confluence pages."
  :group 'extensions)

(defcustom confluence-host ""
  "The host of the Confluence server.
Usually has the form \"orgname.atlassian.net\".
This name is also used to lookup credentials using `authsource'."
  :tag  "Confluence host name"
  :type 'string
  :group 'confluence-reader)

(defcustom confluence-browser-url
  "https://%s/wiki%s"
  "The URL template to open a page in an external browser.
It needs two %s, one for `confluence-host' and one for the rest of
the URL as returned by the API when fetching the page."
  :tag  "Browser URL template"
  :type 'string
  :group 'confluence-reader)

(defcustom confluence-buffer-name-style
  'page-title
  "How to name Confluence page buffers:
* Page ID: \"*Confluence page 12345\"
* Title: \"*Confluence This is a title\""
  :tag  "Buffer naming style"
  :type
  '(choice (const :tag "Page ID" page-id)
           (const :tag "Page title" page-title))
  :group 'confluence-reader)

(defvar-keymap confluence-page-mode-map
  "b" #'bookmark-set
  "C-i" #'shr-next-link
  "C-M-i" #'shr-previous-link
  "<backtab>" #'shr-previous-link
  "o" #'confluence-page--open-in-browser)

(define-derived-mode confluence-page-mode special-mode "Confluence page mode"
  "Mode for reading Confluence pages.
After setting the mode, call `confluence-page-render' to display
a v2 API page response.

\\{confluence-page-mode-map}"
  (setq-local revert-buffer-function #'confluence-page-reload)
  ;; The following local variables are set in `confluence-page-render':
  ;; link target to use to open the page in the browser
  (make-local-variable 'confluence-page--browser-link)
  ;; alist with the page info, to bookmark it
  (make-local-variable 'confluence-page--bookmark)
  ;; page id, used in `confluence-page-reload'
  (make-local-variable 'confluence-page--page-id)
  (setq-local imenu-create-index-function
              #'confluence-page--collect-imenu-items))

(defun confluence-page-render (page-data)
  "Render PAGE-DATA, a Confluence API response."
  (let ((inhibit-read-only t)
        (page-id (gethash "id" page-data))
        (page-title (gethash "title" page-data))
        (browser-link (gethash "webui" (gethash "_links" page-data)))
        ;; deep in the response, is the actual HTML
        (page-html (gethash "value"
                            (gethash "export_view"
                                     (gethash "body" page-data))))
        ;; define customizations to HTML rendering that apply to
        ;; only to this mode
        (shr-table-corner ?\+)
        (shr-table-horizontal-line ?\-)
        (shr-table-vertical-line ?\|)
        (shr-external-rendering-functions (append
                                           shr-external-rendering-functions
                                           '((img . confluence--shr-image)
                                             (a . confluence--shr-a-tag)))))
    (erase-buffer) ;; in case of existing content (reload)
    (insert "<title>" page-title "</title>")
    (insert page-html)
    (shr-render-region (point-min) (point-max))
    (goto-char (point-min))
    (setq-local confluence-page--page-id page-id)
    (setq-local bookmark-make-record-function
                #'confluence-bookmark-make-record)
    (setq-local confluence-page--bookmark
                `(,page-title
                  ,@(bookmark-make-record-default t)
                  (location . ,(format "Page ID: %s" page-id))
                  (page-id . ,page-id)
                  (handler . confluence-bookmark-jump)))))

(defun confluence-page-reload (&arg no-confirm)
  "Reload the current page.
If NO-CONFIRM is t (interactively, the prefix arg), don't ask for
confirmation.

This command forces a new request to the server."
  (interactive "P")
  (when (or no-confirm
            (yes-or-no-p "Reload the page from Confluence? "))
    (confluence-page-by-id confluence-page--page-id)))

(defun confluence-page--collect-imenu-items ()
  "Return an alist for imenu usage.
Currently this collects the positions of all links and headings in the page."
  ;; This is unpackaged/imenu-eww-headings (see
  ;; https://github.com/alphapapa/unpackaged.el).
  (let ((faces '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6 shr-heading)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (cl-loop for next-pos = (next-single-property-change (point) 'face)
                 while next-pos
                 do (goto-char next-pos)
                 for face = (get-text-property (point) 'face)
                 when (cl-typecase face
                        (list (cl-intersection face faces))
                        (symbol (member face faces)))
                 collect (cons (buffer-substring-no-properties
                                (point)
                                (point-at-eol))
                               (point))
                 and do (forward-line 1))))))

(defvar-keymap confluence--link-keymap
  :doc "Borrowed from `eww-link-keymap', handle intra-Confluence links
using this package."
  :parent shr-map
  "RET" #'confluence--follow-link
  "<mouse-2>" #'confluence--follow-link)

(defun confluence-bookmark-make-record ()
  "Return the bookmark created when displaying the page."
  ;; the reason we create the record in advance, is that in that context we
  ;; have the parsed JSON and have the title, page id, etc in local variables
  ;; (we don't keep the whole parsed JSON around...)
  confluence-page--bookmark)

(defun confluence-bookmark-jump (bookmark)
  "Default BOOKMARK handler for Confluence pages."
  (confluence-page-by-id (bookmark-prop-get bookmark 'page-id)))

(put 'confluence-bookmark-jump 'bookmark-handler-type "Confluence")

(defun confluence--log (&rest to-log)
  "Append TO-LOG to the log buffer. Intended for internal use only."
  (let ((log-buffer (get-buffer-create "*confluence-log*"))
        (text (cl-reduce (lambda (accum elem)
                           (concat accum " " (prin1-to-string elem t)))
                         to-log)))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert text)
      (insert "\n"))))

(defun confluence--get-auth-header ()
  "Return the auth header."
  (if-let ((auth-info (car (auth-source-search :host confluence-host
                                               :require '(:secret)))))
      (cons "Authorization"
            (concat "Basic "
                    (base64-encode-string
                     (format "%s:%s" (plist-get auth-info :user)
                             (auth-info-password auth-info))
                     t)))
    (error "Can't get auth info to call Confluence")))

(defun confluence--request (callback target
                            &optional cbarglist verb headers query-params data)
  "Call TARGET (a URL) with HEADERS, QUERY-PARAMS and DATA using VERB.
The VERB defaults to GET.
CALLBACK is invoked with CBARGLIST when ready."
  (unless data
    (setq data ""))
  (confluence--log "API call - Headers (without auth)" headers)
  (push (confluence--get-auth-header) headers)
  (push (cons "Content-Type" "application/json") headers)
  (let ((url-request-extra-headers headers)
        (url-request-method (or verb "GET"))
        (url-request-data (encode-coding-string data 'utf-8)))
    (when query-params
      (setq target (concat target (confluence--build-querystring query-params))))
    (confluence--log "API call - URL" target)
    (url-retrieve target
                  callback
                  cbarglist)))

(defun confluence--build-querystring (params)
  "Convert PARAMS alist to an encoded query string."
  (concat "?"
          (mapconcat (lambda (pair)
                       (format "%s=%s"
                               (car pair)
                               (url-hexify-string (cdr pair))))
                     params
                     "&")))

(defun confluence--check-200-status ()
  "Check that the current request returned 200-OK.
Will raise an error if `url-http-response-status' isn't 200.
Useful for consumers for `confluence--request'."
  (unless (= 200 url-http-response-status)
    (error "Request to Confluence returned: %s, invalid input?"
           url-http-response-status)))

(defun confluence--search-callback (status &optional cbargs)
  "Process the response of calling the seach endpoint.
STATUS and CBARGS are ignored."
  (confluence--check-200-status)
  (let* ((parsed (json-parse-string (buffer-substring url-http-end-of-headers
                                                      (point-max))))
         (results (gethash "results" parsed)))
    (confluence--vtable
     (format "*Confluence search*")
     '("Title" "Space" "Last Modified" "Page ID")
     (cl-loop for elem across results
              for last-mod = (gethash "lastModified" elem)
              for container = (gethash "resultGlobalContainer" elem)
              for space = (gethash "title" container)
              for content = (gethash "content" elem)
              for pid = (gethash "id" content)
              for title = (gethash "title" content)
              collect
              (list title space last-mod pid))
     '("RET" confluence--search-open-page))))

(defun confluence--search-open-page (element)
  "Open the page under point in a search results.
ELEMENT is the object for the row."
  (interactive)
  (let ((page-id (car (last element))))
    (message "Opening page id %s" page-id)
    (confluence-page-by-id page-id)))

(defun confluence--vtable (buf-name columns objects &optional actions)
  "Pop a BUF-NAME with a `vtable' of COLUMNS listing OBJECTS.
ACTIONS should be a plist of key and command. See the vtable
manual for details."
  (with-current-buffer (get-buffer-create buf-name)
    (require 'vtable)
    (setq buffer-read-only nil)
    (erase-buffer)
    (make-vtable
     :columns columns
     :objects objects
     ;; force fixed width face
     :face 'default
     ;; sort by display text (or, name)
     :sort-by '((0 . ascend))
     :keymap (define-keymap
               "q" #'quit-window)
     :actions actions)
    (setq buffer-read-only t))
  (pop-to-buffer buf-name))

(defun confluence--page-callback (status &optional cbargs)
  "Process the response of calling the page endpoint.
STATUS and CBARGS are ignored."
  (confluence--check-200-status)
  (let* ((parsed (json-parse-string (buffer-substring url-http-end-of-headers
                                                      (point-max))))
        (page-buffer-name (confluence--page-buffer-name parsed)))
    (with-current-buffer (get-buffer-create page-buffer-name)
      (confluence-page-mode)
      (confluence-page-render parsed))
    (pop-to-buffer page-buffer-name)))

(defun confluence--page-buffer-name (page-data)
  "Return the buffer name for the page in PAGE-DATA.
The string returned depends on the customization of
`confluence-buffer-name-style'."
  (if (eq confluence-buffer-name-style 'page-title)
      (format "*Confluence: %s*" (gethash "title" page-data))
    (format "*Confluence: ID %s*" (gethash "id" page-data))))

(defun confluence--shr-image (dom &optional url)
  "Special version of `shr-tag-img' for this package.
shr.el allows customization of tag processing, we want to
download images using the correct security token, and let-binding
`url-request-extra-headers' doesn't work as expected.
DOM is the node for the image tag, and I have no idea what URL is."
  ;; this function takes the only relevant part for our use, direct download
  ;; of an imge tag with a simple "src" attribute.
  (let ((alt (dom-attr dom 'alt))
        (width (shr-string-number (dom-attr dom 'width)))
        (height (shr-string-number (dom-attr dom 'height)))
	    (url (shr-expand-url (or url (shr--preferred-image dom))))
        (start (point-marker)))
    (insert " ")
    (confluence--request
     #'shr-image-fetched
     url
     (list (current-buffer) start (set-marker (make-marker) (point))
           (list :width width :height height)))
    (when (zerop shr-table-depth) ;; We are not in a table.
	  (put-text-property start (point) 'keymap shr-image-map)
	  (put-text-property start (point) 'shr-alt alt)
	  (put-text-property start (point) 'image-url url)
	  (put-text-property start (point) 'image-displayer
			             (shr-image-displayer shr-content-function))
	  (put-text-property start (point) 'help-echo
			             (shr-fill-text
			              (or (dom-attr dom 'title) alt))))))

(defun confluence--shr-a-tag (dom)
  "Special version of `shr-tag-a' for this package.
shr.el allows customization of tag processing, we want to
redirect links within Confluence so they are opened with this
package instead of `browse-url'.
DOM is the node for the A tag."
  ;; heavily based on `eww-tag-a', let shr render the link but
  ;; change the button target for Confluence links
  (let ((start (point))
        (page-id (dom-attr dom 'data-linked-resource-id))
        (is-anchor (string-prefix-p "#" (dom-attr dom 'href))))
    (shr-tag-a dom)
    ;; doing links within the page (anchors) doesn't quite work, so disable
    ;; them and change make them black
    (if is-anchor
        (progn
          (remove-text-properties start (point) '(keymap nil face nil))
          ;; a built-in face that looks different than standard links
          (add-text-properties start (point) '(face dired-symlink)))
      ;; for "real" links...
      (if page-id
          (progn
            (put-text-property start (point)
                               'keymap
                               confluence--link-keymap)
            (put-text-property (or shr-start start) (point)
                               'confluence-page-id
                               page-id)
            (insert (propertize " ➡️" 'help-echo "Confluence link")))
        (insert (propertize " 🔗" 'help-echo "External link"))))))

(defun confluence--follow-link ()
  "Browse to the confluence page under point.
A special text property is added when rendering the page, see
`confluence--shr-a-tag'."
  (interactive)
  (let ((page-id (get-text-property (point) 'confluence-page-id)))
    (if page-id
        (confluence-page-by-id page-id)
      (error "This doesn't seem to be a Confluence link"))))

(defun confluence-page--open-in-browser ()
  "Open the current Confluence page in an external browser.
Uses `browse-url-secondary-browser-function'. The full URL is put
together using `confluence-host' and
`confluence-page--browser-link' in the template
`confluence-browser-url'."
  (interactive)
  (funcall browse-url-secondary-browser-function
           (format confluence-browser-url
                   confluence-host
                   confluence-page--browser-link)))

;;;###autoload
(defun confluence-search (search-terms &optional prefix-arg)
  "Look for SEARCH-TERMS in Confluence.
By default this function assumes you are doing a looking for a
text string, and uses the CQL \"text ~ your-search-terms\".
Invoke with PREFIX-ARG to pass the SEARCH-TERMS literally to
Confluence, in other words, to manually type CQL.
Shows the results in a new buffer."
  (interactive "sSearch terms: \nP")
  (unless prefix-arg
    (setf search-terms (format "text ~ \"%s\"" search-terms)))
  (confluence--request #'confluence--search-callback
                       (format "https://%s/wiki/rest/api/search"
                               confluence-host)
                       nil
                       "GET"
                       nil
                       `(("cql". ,search-terms)
                         ("limit" . "100"))))

;;;###autoload
(defun confluence-page-by-id (page-id)
  "Load PAGE-ID from Confluence."
  (interactive "sPage ID: ")
  (confluence--request #'confluence--page-callback
                       (format "https://%s/wiki/api/v2/pages/%s"
                               confluence-host
                               page-id)
                       nil
                       "GET"
                       nil
                       '(("body-format". "export_view"))))

;;;###autoload
(defun confluence-page-from-url (full-url)
  "Load a page from a FULL-URL.
Basically copy from the browser's address bar and this will
extract the id and call `confluence-page-by-id' for you."
  (interactive "sURL: ")
  ;; this is somewhat finicky I guess. Maybe url-parse can help
  (confluence-page-by-id (nth 6 (string-split full-url "/" t))))

(provide 'confluence-reader)

;;; confluence-reader.el ends here
