;; load default erlang templates
(load "erlang-skels")

; I prefer "%% ---..." before "%%%---..."
(defun my-erlang-skel-separator (&optional percent)
  "Return a comment separator."
  (let ((percent (or percent 2)))
    (concat (make-string percent ?%)
            " "
            (make-string (- 71 percent) ?-)
            "\n")))

; I prefer "%% ===..." before "%%%===..."
(defun my-erlang-skel-double-separator (&optional percent)
  "Return a double line (equals sign) comment separator."
  (let ((percent (or percent 2)))
    (concat (make-string percent ?%)
            " "
            (make-string (- 71 percent) ?=)
            "\n")))

; I prefer file level comments to begin with %%, not %%%
(setq erlang-skel-copyright-comment
  (if (boundp '*copyright-organization*)
      '(& "%% @copyright (C) " (format-time-string "%Y") ", "
          *copyright-organization*  n)
      '(& "%% @copyright (C) " (format-time-string "%Y") ", "
          (user-full-name)  n)))

(setq erlang-skel-author-comment
  '(& "%% @author " (user-full-name) " <" erlang-skel-mail-address ">" n))

(setq erlang-skel-lgpl-comment
      '("%% This library is free software; you can redistribute it and/or modify" n
	"%% it under the terms of the GNU Lesser General Public License as" n
	"%% published by the Free Software Foundation; either version 2 of the" n
	"%% License, or (at your option) any later version." n
	"%%" n
	"%% This library is distributed in the hope that it will be useful, but" n
	"%% WITHOUT ANY WARRANTY; without even the implied warranty of" n
	"%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU" n
	"%% Lesser General Public License for more details." n
	"%%" n
	"%% You should have received a copy of the GNU Lesser General Public" n
	"%% License along with this library; if not, write to the Free Software" n
	"%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307" n
	"%% USA" n))

(setq erlang-skel-apache2-comment
      '("%% Licensed under the Apache License, Version 2.0 (the \"License\"); you may" n
        "%% not use this file except in compliance with the License. You may obtain" n
        "%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>" n
        "%%" n
        "%% Unless required by applicable law or agreed to in writing, software" n
        "%% distributed under the License is distributed on an \"AS IS\" BASIS," n
        "%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied." n
        "%% See the License for the specific language governing permissions and" n
        "%% limitations under the License." n))

;; Use Apache 2.0 license by default
(setq erlang-skel-license-comment erlang-skel-apache2-comment)

;; Don't insert a newline by default in the exports list
(setq erlang-skel-export
  '(& "-export([])." n))

;; Always add doc and export to the module, but no vsn
(setq erlang-skel-small-header
  '(o "%% @doc" n n
      (erlang-skel-include erlang-skel-module)
      n
      "-include_lib(\"eunit/include/eunit.hrl\")." n
      n
      (erlang-skel-include erlang-skel-export)
      n)
  )

;; Add a leading separator line, but skip the created-comment
(setq erlang-skel-normal-header
  '(o (my-erlang-skel-double-separator)
      (erlang-skel-include erlang-skel-author-comment)
      (erlang-skel-include erlang-skel-copyright-comment)
      (erlang-skel-include erlang-skel-small-header) n)
  )

;; Add a leading separator line, but skip the created-comment
(setq erlang-skel-large-header
  '(o (my-erlang-skel-double-separator)
      (erlang-skel-include erlang-skel-license-comment)
      "%% " n
      (erlang-skel-include erlang-skel-author-comment)
      (erlang-skel-include erlang-skel-copyright-comment)
      (erlang-skel-include erlang-skel-small-header) )
  )
