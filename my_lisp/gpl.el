(require 'tempo)

(tempo-define-template "gpl-text" '( comment-start "This file is part of " (r "Program name: " name) "." n 
				      comment-start (s name) " is free software; you can redistribute it and/or modify" n
				      comment-start "it under the terms of the GNU General Public License as published by" n
				      comment-start "the Free Software Foundation; either version 2 of the License, or" n
				      comment-start "(at your option) any later version." n
				      comment-start "" (s name) " is distributed in the hope that it will be useful," n
				      comment-start "but WITHOUT ANY WARRANTY; without even the implied warranty of" n
				      comment-start "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the" n
				      comment-start "GNU General Public License for more details." n
				      n
				      comment-start "You should have received a copy of the GNU General Public License" n
				      comment-start "along with " (s name ) "; if not, write to the Free Software" n
				      comment-start "Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA" n

				      comment-start "(C) " (p "Your Name: " ) n))
