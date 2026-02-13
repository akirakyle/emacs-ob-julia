;; ob-julia.el -*- lexical-binding: t -*-

;; Copyright (C) 2026 Akira Kyle

;; Author: Akira Kyle <akira@akirakyle.com>
;; URL: https://github.com/akirakyle/emacs-ob-julia
;; Version: 0.1
;; Package-Requires: ((org) (queue))

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
;; See README.org

;;; Code:

(require 'org)
(require 'org-element)
(require 'ob-core)
(require 'ansi-color)
(require 'queue)

(defgroup ob-julia nil
  "Org Babel Julia via per-session Julia processes using netstrings."
  :group 'org-babel)

(defcustom ob-julia-executable "julia"
  "Julia executable."
  :type 'string)

(defcustom ob-julia-executable-arguments '("--project=.")
  "List of arguments passed to julia executabel."
  :type 'string)

(defvar org-babel-default-header-args:julia
  '((:module . "Main") (:results . "both") (:output-dir . "./.ob-julia/")))

(setq ob-julia--julia-rpc-program
"module ObJulia

function read_netstring(io::IO)
    len_str = String(readuntil(io, UInt8(':')))
    n = parse(Int, len_str)
    n >=0 || error(\"bad netstring: invalid length of %d\", n)
    data = String(read(io, n))
    newline = read(io, UInt8)
    newline == UInt8('\n') || error(\"bad netstring: invalid termination of %s\", newline)
    return data
end

function write_netstring(io::IO, data::AbstractVector{UInt8})
    write(io, string(length(data)))
    write(io, UInt8(':'))
    write(io, data)
    write(io, UInt8('\n'))
end

write_netstring(io::IO, s::AbstractString) = write_netstring(io, Vector{UInt8}(codeunits(s)))
    
function format_result(result, io)
    for mime in (\"image/svg+xml\", \"image/png\")
        if showable(mime, result)
            show(io, mime, result)
            return mime
        end
    end
    isnothing(result) || show(io, \"text/plain\", result)
    return \"text/plain\"
end

catch_abbreviated_backtrace(_) = catch_backtrace()[1:end-27]
    
const NARGS = 2
function handle_eval(code::String, iolimit::String)
    mod = Main
    mkio(io) = IOContext(io, :limit => iolimit == \"true\" ? true : false,
                         :module => mod, :color => true)
    outio_ = Pipe()
    errio_ = Pipe()
    resio_ = IOBuffer()
    excio_ = IOBuffer()
    outio = mkio(outio_)
    errio = mkio(errio_)
    resio = mkio(resio_)
    excio = mkio(excio_)
    mime = \"text/plain\"

    redirect_stdio(stdout=outio, stderr=errio, stdin=devnull) do
        try
            result = Base.include_string(mod, code)
            mime = Core.eval(mod, :(ObJulia.format_result($result, $resio)))
        catch err
            Base.display_error(excio, err, catch_abbreviated_backtrace(excio))
        end
        println(stdout)
        println(stderr)
    end
    stdout_text = String(readavailable(outio_))[1:end-1]
    stderr_text = String(readavailable(errio_))[1:end-1]
    except_text = String(take!(excio_))
    result_text = String(take!(resio_))
    close(outio_)
    close(errio_)

    return (stdout_text, stderr_text, except_text, result_text, mime)
end

function handle_rpc(msg::String)::String
    payload = IOBuffer(msg)
    args = [read_netstring(payload) for _=1:NARGS]
    stdout, stderr, except, result, mime  = handle_eval(args...)
    io = IOBuffer()
    write_netstring(io, stdout)
    write_netstring(io, stderr)
    write_netstring(io, except)
    write_netstring(io, result)
    write_netstring(io, mime)
    return String(take!(io))
end

function handle_stream(ioin::IO, ioout::IO)
    while isopen(ioin) && !eof(ioin)
        write_netstring(ioout, handle_rpc(read_netstring(ioin)))
        flush(ioout)
    end
end

handle_stream(stdin, stdout)

throw(EOFError())

end"
)

(defun ob-julia--escape-string (s)
  (replace-regexp-in-string "\"" "\\\\\"" s))

(defun ob-julia--value-to-julia (value)
  (cond
   ((listp value) (format "\"%s\"" value))
   ((numberp value) value)
   ((stringp value) (or (org-babel--string-to-number value)
                        (concat "\"" (ob-julia--escape-string value) "\"")))
   ((symbolp value) (ob-julia--escape-string (symbol-name value)))
   (t value)))

(defun org-babel-variable-assignments:julia (params)
  (mapcar
   (lambda (pair)
     (format "%s = %s" (car pair) (ob-julia--value-to-julia (cdr pair))))
   (org-babel--get-vars params)))

(defun ob-julia--netstring-encode (s)
  (let* ((bytes (encode-coding-string (or s "") 'utf-8 t)))
    (format "%d:%s\n" (string-bytes bytes) bytes)))

(defun ob-julia--netstring-pop ()
  (goto-char (point-min))
  (when-let ((colon (save-excursion (search-forward ":" nil t))))
    (let* ((len-str (buffer-substring (point-min) (1- colon)))
           (len (string-to-number len-str)))
      (unless (and (integerp len) (>= len 0))
        (error "bad netstring: invalid length of %d" len))
      (let ((need (+ len 1))
            (start colon))
        (when (<= (+ start need) (1+ (buffer-size)))
            (let* ((payload (buffer-substring start (+ start len)))
                   (newline (buffer-substring (+ start len) (+ start len 1))))
              (unless (string= newline "\n")
                (error "bad netstring: invalid termination of %s" newline))
              (delete-region (point-min) (+ start len 1))
              payload))))))

(setq ob-julia--process-prefix "ob-julia:")

(defun ob-julia--process-name (session)
  (format "%s%s" ob-julia--process-prefix session))

(defun ob-julia--process-sentinel (p _event)
  ;;(message "ob-julia--process-sentinel (proc: %s) (event: %s)" p _event)
  (let* ((session (process-get p 'ob-julia-session))
         (buf (process-buffer p)))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun ob-julia--install-filter-and-sentinel (proc session)
  (set-process-coding-system proc 'binary 'binary)
  (set-process-filter proc #'ob-julia--process-filter)
  (set-process-sentinel proc #'ob-julia--process-sentinel)
  (set-process-coding-system proc 'raw-text 'raw-text)
  (process-put proc 'ob-julia-session session)
  (process-put proc 'ob-julia-queue (make-queue))
  proc)

(defun ob-julia--start (session)
  (let* ((process-connection-type nil)
         (tramp-pipe-stty-settings "")
         (tramp-direct-async-process t)
         (proc-name (ob-julia--process-name session))
         (proc-args (append ob-julia-executable-arguments
                            (list "-e" ob-julia--julia-rpc-program)))
         (proc (apply #'start-file-process proc-name (format "*%s*" proc-name)
                      ob-julia-executable proc-args)))
    (ob-julia--install-filter-and-sentinel proc session)))

(defun ob-julia-session-list ()
  (let ((filter-ob-julia (lambda (name)
                           (string-prefix-p ob-julia--process-prefix name)))
        (strip-ob-julia (lambda (name)
                          (string-remove-prefix ob-julia--process-prefix name)))
        (procs (mapcar #'process-name (process-list))))
    (funcall 'mapcar strip-ob-julia (funcall 'seq-filter filter-ob-julia procs))))

(defun ob-julia-interrupt-session (session)
  "Try to interrupt a Julia session."
  (interactive
   (list (completing-read "Interrupt session: " (ob-julia-session-list) nil t)))
  (let* ((proc (get-process (ob-julia--process-name session))))
    (interrupt-process proc)))

(defun ob-julia-stop-session (session)
  "Stop a Julia session."
  (interactive
   (list (completing-read "Stop session: " (ob-julia-session-list) nil t)))
  (let* ((proc (get-process (ob-julia--process-name session))))
    (if (process-live-p proc)
        (delete-process proc)
      (message "No process selected or invalid process name."))))

(defun ob-julia--mime-to-file-ext (mime)
  (cond ((string= mime "image/svg+xml") ".svg")
        ((string= mime "image/png") ".png")
        (t (error "unhandled mime type"))))

(defun ob-julia--temp-file (mime info)
  (let* ((params (nth 2 info))
         (name (nth 4 info))
         (session (cdr (assq :session params)))
         (file-ext (ob-julia--mime-to-file-ext mime))
         (dir (cdr (assq :output-dir params)))
         (fname (if name name (make-temp-name session)))
         (fname (concat fname file-ext)))
    (concat (file-name-as-directory dir) fname)))

(defun ob-julia--handle-result-mime (result mime info)
  (if (string= mime "text/plain")
      result
    (let* ((file (cdr (assq :file (nth 2 info))))
           (file (if file file (ob-julia--temp-file mime info))))
      (write-region result nil file)
      file)))

(defun ob-julia--show-error-pane (stderr)
  (let* ((buf (get-buffer-create "*ob-julia backtrace*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or stderr ""))
        (ansi-color-apply-on-region (point-min) (point-max))
        (compilation-mode)))
    (display-buffer buf)))

(defun ob-julia--apply-ansi-in-last-result ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))

(defun ob-julia--finalize (src-buf src-begin stdout stderr result except mime)
  (with-current-buffer src-buf
    (save-excursion
      (goto-char src-begin)
      (let* ((info (org-babel-get-src-block-info))
             (params (nth 2 info))
             (rparams (cdr (assq :result-params params)))
             (session (cdr (assq :session params)))
             (result (ob-julia--handle-result-mime result mime info))
             (result
              (org-babel-result-cond rparams
                result
                (org-babel-reassemble-table
                 result
                 (org-babel-pick-name (cdr (assq :colname-names params))
                                      (cdr (assq :colnames params)))
                 (org-babel-pick-name (cdr (assq :rowname-names params))
                                      (cdr (assq :rownames params))))))
             (result (cond
                      ((member "output" rparams) (concat stderr stdout))
                      ((member "value" rparams) result)
                      ((member "both" rparams) (concat stderr stdout result)))))
        (unless (or (member "none" rparams) (string-empty-p except))
          (if (member "silent" rparams)
              (message (ansi-color-apply except))
            (ob-julia--show-error-pane except)))
        (when (equal session "none")
          (ob-julia-stop-session "none"))
        (unless (member "none" rparams)
          (org-babel-insert-result result rparams info)
          (ob-julia--apply-ansi-in-last-result)
          (org-link-preview-refresh))))))

(defun ob-julia--process-filter (proc str)
  ;;(message "ob-julia--process-filter from proc %s:" proc) (print str)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert str)
    (when-let ((payload (ob-julia--netstring-pop)))
      (insert payload)
      (let* ((stdout (decode-coding-string (ob-julia--netstring-pop) 'utf-8))
             (stderr (decode-coding-string (ob-julia--netstring-pop) 'utf-8))
             (except (decode-coding-string (ob-julia--netstring-pop) 'utf-8))
             (result (decode-coding-string (ob-julia--netstring-pop) 'utf-8))
             (mime (decode-coding-string (ob-julia--netstring-pop) 'utf-8))
             (req (queue-dequeue (process-get proc 'ob-julia-queue)))
             (buf (plist-get req :buf))
             (src-begin (plist-get req :src-begin)))
        (ob-julia--finalize buf src-begin stdout stderr result except mime)))))

(defun org-babel-execute:julia (body params)
  (let* ((src-begin (make-marker))
         (session (cdr (assq :session params)))
         (iolimit (equal (cdr (assq :iolimit params)) "yes"))
         (var-lines (org-babel-variable-assignments:julia params))
         (code (if var-lines
                   (concat (mapconcat #'identity var-lines "\n")
                           (if (string-match-p "\\`[ \t\n]*\\'" (or body "")) "" "\n")
                           body)
                 body))
         (msg (ob-julia--netstring-encode
               (concat (ob-julia--netstring-encode code)
                       (ob-julia--netstring-encode iolimit))))
         (session (cdr (assq :session params)))
         (proc (or  (get-process (ob-julia--process-name session))
                    (ob-julia--start session))))
    (set-marker src-begin (org-element-property :begin (org-element-context)))
    (queue-enqueue (process-get proc 'ob-julia-queue)
                   (list :buf (current-buffer) :src-begin src-begin))
    (process-send-string proc msg)
    (format "Executing in session %s..." session)))

(provide 'ob-julia)
