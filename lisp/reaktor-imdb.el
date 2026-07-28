;;; reaktor-imdb.el --- IMDB movie entry capture -*- lexical-binding: t -*-

;; Author: Aleksandar Terentić

;;; Commentary:

;; Fetches movie metadata from the IMDB GraphQL API and formats it for
;; org-capture.  `reaktor/imdb--capture-heading' and
;; `reaktor/imdb--capture-body' are the entry points the capture templates
;; call; the heading function does the fetching and stashes the result the
;; body function reads.

;;; Code:

(require 'url)
(require 'seq)
(require 'subr-x)

(defvar reaktor/imdb-genre-tags
  '("action" "anime" "comedy" "cyberpunk" "drama" "erotic" "fantasy"
    "horror" "mystery" "romance" "scifi" "thriller" "trash" "war")
  "Allowed genre tags for movie entries.
IMDB genres are matched against this list (case-insensitive).
Unmatched genres are silently dropped.")

(defun reaktor/imdb--extract-id (url)
  "Extract IMDB title ID (e.g. tt0070034) from URL."
  (when (string-match "tt[0-9]+" url)
    (match-string 0 url)))

(defun reaktor/imdb--fetch-graphql (title-id)
  "Fetch movie data for TITLE-ID via IMDB GraphQL API."
  (let* ((query (format "{title(id:\"%s\"){titleText{text}releaseDate{year}runtime{seconds}genres{genres{text}}plot{plotText{plainText}}directors:credits(first:5,filter:{categories:[\"director\"]}){edges{node{name{nameText{text}}}}}certificate{rating}}}"
                        title-id))
         (url-request-data (json-serialize `((query . ,query))))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (buffer (url-retrieve-synchronously "https://graphql.imdb.com/")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t)
          (let ((json (json-parse-string (buffer-substring (point) (point-max))
                                         :object-type 'alist)))
            (alist-get 'title (alist-get 'data json))))
      (kill-buffer buffer))))

(defun reaktor/imdb--format-duration (seconds)
  "Convert runtime SECONDS to \"1h 39m\" format."
  (when seconds
    (let ((h (/ seconds 3600))
          (m (/ (mod seconds 3600) 60)))
      (string-join
       (delq nil (list (when (> h 0) (format "%sh" h))
                       (when (> m 0) (format "%sm" m))))
       " "))))

(defun reaktor/imdb--map-genres (genre-list)
  "Map GENRE-LIST (list of alists with `text' key) to org tag string."
  (let ((tags (seq-filter
               (lambda (tag)
                 (member tag reaktor/imdb-genre-tags))
               (mapcar (lambda (g)
                         (downcase (replace-regexp-in-string
                                    "[- ]" "" (alist-get 'text g))))
                       genre-list))))
    (if tags
        (concat ":" (string-join tags ":") ":")
      "")))

(defun reaktor/imdb--extract-directors (credits)
  "Extract director names from GraphQL CREDITS edges."
  (mapconcat (lambda (edge)
               (let ((node (alist-get 'node edge)))
                 (alist-get 'text (alist-get 'nameText (alist-get 'name node)))))
             (append (alist-get 'edges credits) nil)
             ", "))

(defvar reaktor/imdb--capture-data nil
  "Plist holding fetched IMDB data for the current capture.")

(defun reaktor/imdb--fetch-for-capture ()
  "Prompt for IMDB URL, fetch data, and store in `reaktor/imdb--capture-data'."
  (let* ((url (read-string "IMDB URL: "))
         (title-id (reaktor/imdb--extract-id url)))
    (unless title-id
      (user-error "Could not extract IMDB title ID from %s" url))
    (let ((data (reaktor/imdb--fetch-graphql title-id)))
      (unless data
        (user-error "Failed to fetch IMDB data for %s" title-id))
      (let* ((genre-list (append (alist-get 'genres (alist-get 'genres data)) nil))
             (genres (reaktor/imdb--map-genres genre-list))
             (stars (read-string "Stars: "))
             (reference (read-string "Reference: "))
             (year (alist-get 'year (alist-get 'releaseDate data)))
             (duration (reaktor/imdb--format-duration
                        (alist-get 'seconds (alist-get 'runtime data))))
             (cert (alist-get 'certificate data))
             (rating (if (and cert (listp cert))
                         (or (alist-get 'rating cert) "")
                       ""))
             (description (alist-get 'plainText
                                     (alist-get 'plotText (alist-get 'plot data))))
             (directors (reaktor/imdb--extract-directors (alist-get 'directors data)))
             (title (alist-get 'text (alist-get 'titleText data)))
             (padding (if (string-empty-p genres) "" "  "))
             (body (string-join
                    (delq nil
                          (list (when (and description (not (string-empty-p description)))
                                  description)
                                (when (and directors (not (string-empty-p directors)))
                                  (format "Director: %s" directors))
                                (when (not (string-empty-p stars))
                                  (format "Stars: %s" stars))
                                (when (not (string-empty-p reference))
                                  (format "Reference: %s" reference))))
                    "\n")))
        (setq reaktor/imdb--capture-data
              (list :heading (format "WANT [[%s][%s]] | %s | %s | %s%s%s"
                                     url title
                                     (if year (number-to-string year) "")
                                     (or duration "") rating
                                     padding genres)
                    :body body))))))

(defun reaktor/imdb--capture-heading ()
  "Fetch IMDB data and return heading for capture template."
  (reaktor/imdb--fetch-for-capture)
  (plist-get reaktor/imdb--capture-data :heading))

(defun reaktor/imdb--capture-body ()
  "Return the body for the current IMDB capture."
  (plist-get reaktor/imdb--capture-data :body))

(provide 'reaktor-imdb)
;;; reaktor-imdb.el ends here
