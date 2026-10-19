;;; soccer-scores.el --- Live soccer scores with a favorite club (AC Milan) -*- lexical-binding: t; -*-

;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games, comm
;; Forza Milan!

;;; Commentary:

;; Check live soccer scores from inside Emacs, with AC Milan front and center.
;; Data comes from ESPN's public (unofficial) scoreboard JSON, so no API key
;; is needed.  Being unofficial, it may change without notice.
;;
;; Commands:
;;   M-x soccer-scores          All of today's matches in `soccer-scores-leagues'.
;;   M-x soccer-scores-milan    Only your favorite club: recent results,
;;                              live games, and upcoming fixtures.
;;   M-x soccer-scores-standings
;;                              League table for any configured league.
;;   M-x soccer-scores-mode-line-mode
;;                              Show the live Milan score in the mode line and
;;                              get a GOAL message when the score changes.
;;
;; In the scores buffer:  g refresh   m toggle all/favorite   q quit
;;
;; Setup:
;;   (add-to-list 'load-path "/path/to/dir")
;;   (require 'soccer-scores)
;;   (global-set-key (kbd "C-c m") #'soccer-scores-milan)

;;; Code:

(require 'url)
(require 'seq)
(require 'subr-x)
(require 'cl-lib)

(defconst soccer-scores-version "0.4.0")

(defgroup soccer-scores nil
  "Live soccer scores."
  :group 'comm
  :prefix "soccer-scores-")

(defcustom soccer-scores-favorite-team "AC Milan"
  "Exact ESPN display name of your favorite team (case-insensitive).
Exact matching matters: \"Inter Milan\" must not match \"AC Milan\"."
  :type 'string)

(defcustom soccer-scores-favorite-abbrev "MIL"
  "Short name used for your favorite team in the mode line."
  :type 'string)

(defcustom soccer-scores-leagues
  '(("ita.1"            . "Serie A")
    ("uefa.champions"   . "Champions League")
    ("uefa.europa"      . "Europa League")
    ("ita.coppa_italia" . "Coppa Italia")
    ("eng.1"            . "Premier League")
    ("esp.1"            . "La Liga")
    ("ger.1"            . "Bundesliga")
    ("fra.1"            . "Ligue 1"))
  "Alist of (ESPN-LEAGUE-SLUG . DISPLAY-NAME) to query."
  :type '(alist :key-type string :value-type string))

(defcustom soccer-scores-refresh-interval 60
  "Seconds between automatic refreshes of the scores buffer and mode line."
  :type 'integer)

(defcustom soccer-scores-favorite-leagues
  '("ita.1" "uefa.champions" "uefa.europa" "ita.coppa_italia")
  "ESPN league slugs searched for your favorite team's matches.
Each league costs one request per day in the window, so keep it short."
  :type '(repeat string))

(defcustom soccer-scores-standings-cache-ttl 600
  "Seconds to cache a league table."
  :type 'integer)

(defcustom soccer-scores-cache-ttl 3600
  "Seconds to cache scoreboard data for days other than today."
  :type 'integer)

(defcustom soccer-scores-favorite-days-back 3
  "Days of past results to show in the favorite-team view."
  :type 'integer)

(defcustom soccer-scores-favorite-days-ahead 10
  "Days of upcoming fixtures to show in the favorite-team view."
  :type 'integer)

(defface soccer-scores-league
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for league headings.")

(defface soccer-scores-live
  '((t :inherit success :weight bold))
  "Face for the clock of a live match.")

(defface soccer-scores-favorite
  '((t :inherit error :weight bold))
  "Face for your favorite team.")

(defface soccer-scores-dim
  '((t :inherit shadow))
  "Face for finished or upcoming matches.")

;;;; Fetching and parsing

(defun soccer-scores--url (league &optional date)
  "Return the scoreboard URL for LEAGUE, optionally for one DATE (YYYYMMDD)."
  (format "https://site.api.espn.com/apis/site/v2/sports/soccer/%s/scoreboard%s"
          league (if date (concat "?dates=" date) "")))

(defun soccer-scores--parse-time (str)
  "Parse ESPN's ISO time STR (which may omit seconds) into an Emacs time."
  (when (stringp str)
    (when (string-match-p "T[0-9][0-9]:[0-9][0-9]Z\\'" str)
      (setq str (concat (substring str 0 -1) ":00Z")))
    (ignore-errors (date-to-time str))))

(defun soccer-scores--parse-event (ev league-name)
  "Turn ESPN event EV from LEAGUE-NAME into a plist."
  (let* ((comp (car (alist-get 'competitions ev)))
         (cs   (alist-get 'competitors comp))
         (find (lambda (side)
                 (seq-find (lambda (c) (equal (alist-get 'homeAway c) side)) cs)))
         (home (funcall find "home"))
         (away (funcall find "away"))
         (type (alist-get 'type (alist-get 'status ev))))
    (when (and home away)
      (list :id (alist-get 'id ev)
            :league league-name
            :home (alist-get 'displayName (alist-get 'team home))
            :away (alist-get 'displayName (alist-get 'team away))
            :home-abbrev (alist-get 'abbreviation (alist-get 'team home))
            :away-abbrev (alist-get 'abbreviation (alist-get 'team away))
            :home-score (or (alist-get 'score home) "0")
            :away-score (or (alist-get 'score away) "0")
            :state (alist-get 'state type)          ; "pre", "in" or "post"
            :detail (or (alist-get 'shortDetail type) "")
            :time (soccer-scores--parse-time (alist-get 'date ev))))))

(defun soccer-scores--parse-response (status league)
  "Parse the current url.el response buffer into a list of match plists.
STATUS is the url-retrieve status; LEAGUE is a (SLUG . NAME) cons."
  (unwind-protect
      (unless (plist-get status :error)
        (goto-char (point-min))
        (when (re-search-forward "\r?\n\r?\n" nil t)
          (let ((json (ignore-errors
                        (json-parse-buffer :object-type 'alist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object nil))))
            (delq nil
                  (mapcar (lambda (ev) (soccer-scores--parse-event ev (cdr league)))
                          (alist-get 'events json))))))
    (kill-buffer (current-buffer))))

(defun soccer-scores--fetch (leagues dates callback)
  "Fetch LEAGUES in parallel for DATES; call CALLBACK with all matches."
  (let ((pending (length leagues))
        (results nil))
    (dolist (league leagues)
      (let ((league league))
        (url-retrieve
         (soccer-scores--url (car league) dates)
         (lambda (status)
           (setq results (nconc results (soccer-scores--parse-response status league)))
           (when (zerop (cl-decf pending))
             (funcall callback results)))
         nil t t)))))

(defun soccer-scores--fav-name-p (name abbrev)
  "Non-nil if NAME or ABBREV identifies the favorite team."
  (or (string-equal-ignore-case soccer-scores-favorite-team (or name ""))
      (and abbrev (string-equal-ignore-case soccer-scores-favorite-abbrev abbrev))))

(defun soccer-scores--favorite-p (match)
  "Non-nil if MATCH involves the favorite team."
  (or (soccer-scores--fav-name-p (plist-get match :home) (plist-get match :home-abbrev))
      (soccer-scores--fav-name-p (plist-get match :away) (plist-get match :away-abbrev))))

;;;; Favorite-team window (one request per league per day)

(defvar soccer-scores--day-cache (make-hash-table :test 'equal)
  "Key \"slug|date\" -> (TIMESTAMP . MATCHES).")

(defun soccer-scores--favorite-jobs ()
  "Return a list of (LEAGUE DATE TODAY-P) covering the favorite window."
  (let (jobs)
    (dolist (slug soccer-scores-favorite-leagues)
      (let ((league (or (assoc slug soccer-scores-leagues) (cons slug slug))))
        (cl-loop for d from (- soccer-scores-favorite-days-back)
                 to soccer-scores-favorite-days-ahead
                 do (push (list league
                                (format-time-string
                                 "%Y%m%d" (time-add (current-time) (* d 86400)))
                                (= d 0))
                          jobs))))
    (nreverse jobs)))

(defun soccer-scores--dedupe (matches)
  "Remove duplicate MATCHES by event id."
  (cl-remove-duplicates matches :key (lambda (m) (plist-get m :id))
                        :test #'equal :from-end t))

(defun soccer-scores--fetch-favorite (callback)
  "Fetch the favorite window (using the cache) and call CALLBACK with matches."
  (let ((results nil) (todo nil) (now (float-time)))
    (dolist (job (soccer-scores--favorite-jobs))
      (let* ((key (format "%s|%s" (car (nth 0 job)) (nth 1 job)))
             (hit (gethash key soccer-scores--day-cache)))
        (if (and hit (not (nth 2 job))
                 (< (- now (car hit)) soccer-scores-cache-ttl))
            (setq results (append results (cdr hit)))
          (push (cons key job) todo))))
    (let ((pending (length todo)))
      (if (zerop pending)
          (funcall callback (soccer-scores--dedupe results))
        (dolist (entry todo)
          (let ((key (car entry))
                (league (nth 0 (cdr entry)))
                (date (nth 1 (cdr entry))))
            (url-retrieve
             (soccer-scores--url (car league) date)
             (lambda (status)
               (let ((ms (soccer-scores--parse-response status league)))
                 (unless (plist-get status :error)
                   (puthash key (cons (float-time) ms) soccer-scores--day-cache))
                 (setq results (append results ms))
                 (when (zerop (cl-decf pending))
                   (funcall callback (soccer-scores--dedupe results)))))
             nil t t)))))))

;;;; Rendering

(defvar-local soccer-scores--filter 'all
  "Either `all' or `favorite' in a scores buffer.")

(defconst soccer-scores--buffer-name "*Soccer Scores*")

(defun soccer-scores--team (name score fav)
  "Propertize team NAME and SCORE; highlight when FAV."
  (let ((txt (format "%s %s" name score)))
    (if fav (propertize txt 'face 'soccer-scores-favorite) txt)))

(defun soccer-scores--insert-match (m &optional with-league)
  "Insert one line for match M, prefixed by its league if WITH-LEAGUE."
  (let* ((state (plist-get m :state))
         (live (equal state "in"))
         (pre (equal state "pre"))
         (home (plist-get m :home))
         (away (plist-get m :away))
         (fav-h (soccer-scores--fav-name-p home (plist-get m :home-abbrev)))
         (fav-a (soccer-scores--fav-name-p away (plist-get m :away-abbrev)))
         (when-str (if (plist-get m :time)
                       (format-time-string "%a %d %b %H:%M" (plist-get m :time))
                     ""))
         (status (cond (live (propertize (plist-get m :detail) 'face 'soccer-scores-live))
                       (pre (propertize "kick-off" 'face 'soccer-scores-dim))
                       (t (propertize (plist-get m :detail) 'face 'soccer-scores-dim)))))
    (insert
     (format "  %-15s " (propertize when-str 'face 'soccer-scores-dim))
     (if pre
         (format "%s  vs  %s"
                 (soccer-scores--team home "" fav-h)
                 (soccer-scores--team away "" fav-a))
       (format "%s  -  %s"
               (soccer-scores--team home (plist-get m :home-score) fav-h)
               (soccer-scores--team away (plist-get m :away-score) fav-a)))
     "   " status "\n")))

(defun soccer-scores--time< (a b)
  "Non-nil if match A kicks off before match B."
  (time-less-p (or (plist-get a :time) 0) (or (plist-get b :time) 0)))

(defun soccer-scores--render-favorite (matches)
  "Insert MATCHES grouped as Live, Upcoming and Recent results."
  (let ((live (seq-filter (lambda (m) (equal (plist-get m :state) "in")) matches))
        (upcoming (sort (seq-filter (lambda (m) (equal (plist-get m :state) "pre")) matches)
                        #'soccer-scores--time<))
        (recent (sort (seq-filter (lambda (m) (equal (plist-get m :state) "post")) matches)
                      (lambda (a b) (soccer-scores--time< b a)))))
    (cl-loop for (title . ms) in `(("Live now" . ,live)
                                   ("Upcoming" . ,upcoming)
                                   ("Recent results" . ,recent))
             when ms
             do (insert (propertize title 'face 'soccer-scores-league) "\n")
             (dolist (m ms) (soccer-scores--insert-match m t))
             (insert "\n"))))

(defun soccer-scores--render (matches)
  "Render MATCHES into the scores buffer."
  (when-let ((buf (get-buffer soccer-scores--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (pt (point))
            (matches (if (eq soccer-scores--filter 'favorite)
                         (seq-filter #'soccer-scores--favorite-p matches)
                       matches)))
        (erase-buffer)
        (insert (propertize
                 (if (eq soccer-scores--filter 'favorite)
                     (format "%s  (live, upcoming, results)  [soccer-scores v%s]\n"
                             soccer-scores-favorite-team soccer-scores-version)
                   (format "Soccer scores today  [soccer-scores v%s]\n" soccer-scores-version))
                 'face '(:weight bold :height 1.3))
                (propertize (format "Updated %s    g: refresh   m: toggle %s only   t: league table   q: quit\n\n"
                                    (format-time-string "%H:%M:%S")
                                    soccer-scores-favorite-team)
                            'face 'soccer-scores-dim))
        (cond
         ((null matches)
          (insert "  No matches found.\n"))
         ((eq soccer-scores--filter 'favorite)
          (soccer-scores--render-favorite matches))
         (t
          (dolist (league soccer-scores-leagues)
            (let ((ms (seq-filter (lambda (m) (equal (plist-get m :league) (cdr league)))
                                  matches)))
              (when ms
                (insert (propertize (cdr league) 'face 'soccer-scores-league) "\n")
                (dolist (m (sort ms #'soccer-scores--time<))
                  (soccer-scores--insert-match m))
                (insert "\n"))))))
        (goto-char (min pt (point-max)))))))

;;;; Buffer, mode and refresh

(defvar soccer-scores--timer nil)

(defun soccer-scores-refresh ()
  "Refresh the scores buffer."
  (interactive)
  (when-let ((buf (get-buffer soccer-scores--buffer-name)))
    (let ((fav (eq (buffer-local-value 'soccer-scores--filter buf) 'favorite))
          (done (lambda (matches)
                  (soccer-scores--render matches)
                  (message ""))))
      (message "Fetching scores...")
      (if fav
          (soccer-scores--fetch-favorite done)
        (soccer-scores--fetch soccer-scores-leagues nil done)))))

(defun soccer-scores-toggle-favorite ()
  "Toggle between all matches and favorite-team matches."
  (interactive)
  (setq soccer-scores--filter (if (eq soccer-scores--filter 'all) 'favorite 'all))
  (soccer-scores-refresh))

(defun soccer-scores--auto-refresh ()
  "Refresh the buffer if it is alive; otherwise stop the timer."
  (if (get-buffer soccer-scores--buffer-name)
      (soccer-scores-refresh)
    (when (timerp soccer-scores--timer)
      (cancel-timer soccer-scores--timer)
      (setq soccer-scores--timer nil))))

(defvar soccer-scores-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" #'soccer-scores-refresh)
    (define-key map "m" #'soccer-scores-toggle-favorite)
    (define-key map "t" #'soccer-scores-standings)
    map))

(define-derived-mode soccer-scores-mode special-mode "Soccer"
  "Major mode for viewing live soccer scores."
  (setq truncate-lines t))

(defun soccer-scores--open (filter)
  "Open the scores buffer with FILTER (`all' or `favorite')."
  (let ((buf (get-buffer-create soccer-scores--buffer-name)))
    (with-current-buffer buf
      (soccer-scores-mode)
      (setq soccer-scores--filter filter)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Loading...\n")))
    (pop-to-buffer buf)
    (unless (timerp soccer-scores--timer)
      (setq soccer-scores--timer
            (run-with-timer soccer-scores-refresh-interval
                            soccer-scores-refresh-interval
                            #'soccer-scores--auto-refresh)))
    (soccer-scores-refresh)))

;;;###autoload
(defun soccer-scores ()
  "Show today's soccer scores for all configured leagues."
  (interactive)
  (soccer-scores--open 'all))

;;;###autoload
(defun soccer-scores-milan ()
  "Show recent results, live games and fixtures for the favorite team."
  (interactive)
  (soccer-scores--open 'favorite))

;;;; League tables

(defconst soccer-scores--standings-buffer "*Soccer Standings*")

(defvar soccer-scores--standings-cache (make-hash-table :test 'equal)
  "League slug -> (TIMESTAMP . GROUPS).")

(defvar-local soccer-scores--standings-league nil
  "The (SLUG . NAME) league shown in a standings buffer.")

(defun soccer-scores--standings-url (slug)
  "Return the ESPN standings URL for league SLUG."
  (format "https://site.api.espn.com/apis/v2/sports/soccer/%s/standings" slug))

(defun soccer-scores--stat (stats name)
  "Return the integer value of stat NAME in the STATS list, or nil."
  (let* ((stat (seq-find (lambda (x) (equal (alist-get 'name x) name)) stats))
         (v (and stat (alist-get 'value stat))))
    (and (numberp v) (truncate v))))

(defun soccer-scores--standings-rows (entries)
  "Convert ESPN standings ENTRIES into a list of row plists sorted by rank."
  (let ((rows
         (cl-loop
          for e in entries
          for i from 1
          collect
          (let* ((team (alist-get 'team e))
                 (stats (alist-get 'stats e))
                 (g (lambda (n) (soccer-scores--stat stats n))))
            (list :rank (or (funcall g "rank") i)
                  :team (alist-get 'displayName team)
                  :abbrev (alist-get 'abbreviation team)
                  :gp (funcall g "gamesPlayed")
                  :w (funcall g "wins")
                  :d (funcall g "ties")
                  :l (funcall g "losses")
                  :gf (funcall g "pointsFor")
                  :ga (funcall g "pointsAgainst")
                  :gd (funcall g "pointDifferential")
                  :pts (funcall g "points"))))))
    (sort rows (lambda (a b) (< (plist-get a :rank) (plist-get b :rank))))))

(defun soccer-scores--standings-groups (json)
  "Return a list of (GROUP-NAME . ROWS) extracted from standings JSON."
  (let ((children (alist-get 'children json)))
    (if children
        (delq nil
              (mapcar (lambda (c)
                        (let ((entries (alist-get 'entries (alist-get 'standings c))))
                          (when entries
                            (cons (alist-get 'name c)
                                  (soccer-scores--standings-rows entries)))))
                      children))
      (let ((entries (alist-get 'entries (alist-get 'standings json))))
        (when entries
          (list (cons nil (soccer-scores--standings-rows entries))))))))

(defun soccer-scores--standings-parse (status)
  "Parse the current response buffer into standings groups, or nil.
STATUS is the url-retrieve status."
  (unwind-protect
      (unless (plist-get status :error)
        (goto-char (point-min))
        (when (re-search-forward "\r?\n\r?\n" nil t)
          (let ((json (ignore-errors
                        (json-parse-buffer :object-type 'alist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object nil))))
            (and json (soccer-scores--standings-groups json)))))
    (kill-buffer (current-buffer))))

(defun soccer-scores--standings-fetch (slug callback &optional force)
  "Fetch the table for league SLUG and call CALLBACK with its groups.
Uses the cache unless FORCE is non-nil."
  (let ((hit (gethash slug soccer-scores--standings-cache)))
    (if (and hit (not force)
             (< (- (float-time) (car hit)) soccer-scores-standings-cache-ttl))
        (funcall callback (cdr hit))
      (url-retrieve
       (soccer-scores--standings-url slug)
       (lambda (status)
         (let ((groups (soccer-scores--standings-parse status)))
           (when groups
             (puthash slug (cons (float-time) groups)
                      soccer-scores--standings-cache))
           (funcall callback groups)))
       nil t t))))

(defun soccer-scores--n (v)
  "Format number V, or a dash when it is nil."
  (if v (number-to-string v) "-"))

(defun soccer-scores--standings-line (r)
  "Return the table line for row plist R."
  (let* ((fav (soccer-scores--fav-name-p (plist-get r :team) (plist-get r :abbrev)))
         (gd (plist-get r :gd))
         (line (format "%3s  %-24s %3s %3s %3s %3s %4s %4s %4s %4s\n"
                       (soccer-scores--n (plist-get r :rank))
                       (truncate-string-to-width (or (plist-get r :team) "?") 24)
                       (soccer-scores--n (plist-get r :gp))
                       (soccer-scores--n (plist-get r :w))
                       (soccer-scores--n (plist-get r :d))
                       (soccer-scores--n (plist-get r :l))
                       (soccer-scores--n (plist-get r :gf))
                       (soccer-scores--n (plist-get r :ga))
                       (if gd (format "%+d" gd) "-")
                       (soccer-scores--n (plist-get r :pts)))))
    (if fav (propertize line 'face 'soccer-scores-favorite) line)))

(defun soccer-scores--standings-render (league groups)
  "Render GROUPS for LEAGUE into the standings buffer."
  (when-let ((buf (get-buffer soccer-scores--standings-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (pt (point)))
        (erase-buffer)
        (insert (propertize (format "%s standings  [soccer-scores v%s]\n"
                                    (cdr league) soccer-scores-version)
                            'face '(:weight bold :height 1.3))
                (propertize (format "Updated %s    g: refresh   n/p: next/prev league   l: choose league   q: quit\n\n"
                                    (format-time-string "%H:%M:%S"))
                            'face 'soccer-scores-dim))
        (if (null groups)
            (insert "  No table available (request failed, or ESPN has none for this competition).\n")
          (dolist (g groups)
            (when (car g)
              (insert (propertize (car g) 'face 'soccer-scores-league) "\n"))
            (insert (propertize
                     (format "%3s  %-24s %3s %3s %3s %3s %4s %4s %4s %4s\n"
                             "#" "Team" "P" "W" "D" "L" "GF" "GA" "GD" "Pts")
                     'face '(:weight bold :underline t)))
            (dolist (r (cdr g))
              (insert (soccer-scores--standings-line r)))
            (insert "\n")))
        (goto-char (min pt (point-max)))))))

(defun soccer-scores--read-league ()
  "Prompt for a league from `soccer-scores-leagues'; return (SLUG . NAME)."
  (let* ((default (cdr (or (assoc (car soccer-scores-favorite-leagues)
                                  soccer-scores-leagues)
                           (car soccer-scores-leagues))))
         (choice (completing-read "League table: "
                                  (mapcar #'cdr soccer-scores-leagues)
                                  nil t nil nil default)))
    (rassoc choice soccer-scores-leagues)))

(defvar soccer-scores-standings-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" #'soccer-scores-standings-refresh)
    (define-key map "n" #'soccer-scores-standings-next)
    (define-key map "p" #'soccer-scores-standings-previous)
    (define-key map "l" #'soccer-scores-standings)
    map))

(define-derived-mode soccer-scores-standings-mode special-mode "Standings"
  "Major mode for viewing soccer league tables."
  (setq truncate-lines t))

;;;###autoload
(defun soccer-scores-standings (&optional league force)
  "Show the league table for LEAGUE, a (SLUG . NAME) cons.
Interactively, prompt for the league.  FORCE bypasses the cache."
  (interactive (list (soccer-scores--read-league)))
  (let ((league (or league (car soccer-scores-leagues)))
        (buf (get-buffer-create soccer-scores--standings-buffer)))
    (with-current-buffer buf
      (soccer-scores-standings-mode)
      (setq soccer-scores--standings-league league)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Loading %s table...\n" (cdr league)))))
    (pop-to-buffer buf)
    (soccer-scores--standings-fetch
     (car league)
     (lambda (groups) (soccer-scores--standings-render league groups))
     force)))

(defun soccer-scores-standings-refresh ()
  "Refetch the current league table, ignoring the cache."
  (interactive)
  (soccer-scores-standings soccer-scores--standings-league t))

(defun soccer-scores--standings-step (delta)
  "Show the league DELTA positions away from the current one."
  (let* ((leagues soccer-scores-leagues)
         (pos (or (cl-position (car soccer-scores--standings-league) leagues
                               :key #'car :test #'equal)
                  0))
         (next (nth (mod (+ pos delta) (length leagues)) leagues)))
    (soccer-scores-standings next)))

(defun soccer-scores-standings-next ()
  "Show the next league's table."
  (interactive)
  (soccer-scores--standings-step 1))

(defun soccer-scores-standings-previous ()
  "Show the previous league's table."
  (interactive)
  (soccer-scores--standings-step -1))

;;;; Mode line ticker and goal alerts

(defvar soccer-scores-mode-line-string ""
  "Mode line text for the favorite team's current match.")
(put 'soccer-scores-mode-line-string 'risky-local-variable t)

(defvar soccer-scores--ml-timer nil)
(defvar soccer-scores--last-scores (make-hash-table :test 'equal)
  "Match id -> last seen \"H-A\" score, used to detect goals.")

(defun soccer-scores--short (m)
  "Short mode-line string for match M."
  (let ((fav-home (string-equal-ignore-case soccer-scores-favorite-team
                                            (or (plist-get m :home) ""))))
    (format "%s %s-%s %s%s"
            (plist-get m :home-abbrev)
            (plist-get m :home-score)
            (plist-get m :away-score)
            (plist-get m :away-abbrev)
            (if (equal (plist-get m :state) "in")
                (concat " " (plist-get m :detail))
              (ignore fav-home) ""))))

(defun soccer-scores--check-goals (matches)
  "Message a GOAL alert when the score of a live match in MATCHES changed."
  (dolist (m matches)
    (let* ((id (plist-get m :id))
           (score (format "%s-%s" (plist-get m :home-score) (plist-get m :away-score)))
           (old (gethash id soccer-scores--last-scores)))
      (when (and old (not (equal old score)) (equal (plist-get m :state) "in"))
        (message "GOAL! %s" (soccer-scores--short m)))
      (puthash id score soccer-scores--last-scores))))

(defun soccer-scores--mode-line-update ()
  "Fetch today's matches and update the mode line string."
  (soccer-scores--fetch
   soccer-scores-leagues nil
   (lambda (matches)
     (let* ((fav (seq-filter #'soccer-scores--favorite-p matches))
            (live (seq-find (lambda (m) (equal (plist-get m :state) "in")) fav)))
       (soccer-scores--check-goals fav)
       (setq soccer-scores-mode-line-string
             (if live (concat " [" (soccer-scores--short live) "]") ""))
       (force-mode-line-update t)))))

;;;###autoload
(define-minor-mode soccer-scores-mode-line-mode
  "Show the favorite team's live score in the mode line."
  :global t
  (when (timerp soccer-scores--ml-timer)
    (cancel-timer soccer-scores--ml-timer)
    (setq soccer-scores--ml-timer nil))
  (setq soccer-scores-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (if soccer-scores-mode-line-mode
      (progn
        (unless (member '(:eval soccer-scores-mode-line-string) global-mode-string)
          (setq global-mode-string
                (append global-mode-string '((:eval soccer-scores-mode-line-string)))))
        (soccer-scores--mode-line-update)
        (setq soccer-scores--ml-timer
              (run-with-timer soccer-scores-refresh-interval
                              soccer-scores-refresh-interval
                              #'soccer-scores--mode-line-update)))
    (setq global-mode-string
          (delete '(:eval soccer-scores-mode-line-string) global-mode-string))))

;;;; Diagnostics

;;;###autoload
(defun soccer-scores-debug ()
  "Show what ESPN returns for each league/day in the favorite window."
  (interactive)
  (let ((buf (get-buffer-create "*Soccer Scores Debug*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "soccer-scores v%s\nFavorite: %S / %S\n\n"
                      soccer-scores-version soccer-scores-favorite-team
                      soccer-scores-favorite-abbrev)))
    (pop-to-buffer buf)
    (dolist (job (soccer-scores--favorite-jobs))
      (let ((league (nth 0 job)) (date (nth 1 job)))
        (url-retrieve
         (soccer-scores--url (car league) date)
         (lambda (status)
           (let* ((err (plist-get status :error))
                  (http (progn (goto-char (point-min))
                               (and (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
                                    (match-string 1))))
                  (all (soccer-scores--parse-response status league))
                  (fav (seq-filter #'soccer-scores--favorite-p all)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (goto-char (point-max))
                 (insert (format "%-18s %s  HTTP %s%s  %d events, %d favorite\n"
                                 (cdr league) date (or http "?")
                                 (if err " ERROR" "") (length all) (length fav)))
                 (dolist (m fav)
                   (insert (format "    %s v %s [%s]\n" (plist-get m :home)
                                   (plist-get m :away) (plist-get m :state))))))))
         nil t t)))))

(provide 'soccer-scores)
;;; soccer-scores.el ends here
