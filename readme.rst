
****************************
Emacs StackExchange Snippets
****************************

Emacs *StackExchange snippets for everyday use!*

Motivation
==========

While copying snippets from the web into your configuration can be OK in a few places.
Manually having to maintain fixes/improvements to small code fragments in multiple places gets tedious.

This repository intends to make the hassle out of selectively
loading useful functionality into your configuration.

Without having to buy into a package which are typically an all-or-nothing choice.


Conventions
===========

Self Contained
   Snippets can be used without depending on other files or packages.
No Configuration
   Snippets should be usable without configuration,
   optional arguments may be useful in some cases.
Documented
   The functionality of each snippet should have clear text explaining what it does.
Hygienic
   All snippets use their file name in global symbols to avoid polluting the global name-space.


Focus
=====

Since Emacs can do many things, these packages are focused on text editing for writers and programmers.

Snippets for handling email, chat, web browsing ... etc are out of scope.


Usage
=====

To use these packages, first checkout this repository into your Emacs user directory,
typically ``~/.emacs.d`` or ``~/.config/emacs`` for Emacs 27 and newer.

Then, select the packages you want to use.


.. code-block:: elisp

   (let ((se-dir (concat user-emacs-directory
                         (file-name-as-directory "emacs-stackexchange-snippets"))))
     (if (file-directory-p se-dir)
         (dolist
             (se-item
              (list
               "stackexchange-mode-line-visual-bell"
               "stackexchange-revert-all-buffers"
               "stackexchange-scratch-buffer-from-file"
               "stackexchange-transpose-args"
               "stackexchange-transpose-words"))
           (load (concat se-dir se-item) :nomessage t))
       (message "StackExchange package missing at %S" se-dir)))


Package List
============

The current list of packages are as follows:

.. BEGIN PACKAGE LIST

`stackexchange-backspace-whitespace-to-tab-stop <https://emacs.stackexchange.com/a/56084>`__
   Backspace to tab-stop or single character.
`stackexchange-c-transpose-args <https://emacs.stackexchange.com/a/47934>`__
   Transpose comma separated arguments (C family languages).
`stackexchange-compile-context-sensitive <https://emacs.stackexchange.com/a/56008>`__
   Context sensitive build command.
`stackexchange-delete-surround-at-point <https://emacs.stackexchange.com/a/54679>`__
   Delete surrounding brackets.
`stackexchange-frame-urgent-hint-set <https://emacs.stackexchange.com/a/56037>`__
   Visual bell using mode-line.
`stackexchange-ispell-word-immediate <https://emacs.stackexchange.com/a/55545>`__
   Correct spelling without prompting.
`stackexchange-mode-line-visual-bell <https://emacs.stackexchange.com/a/55988>`__
   Visual bell using mode line.
`stackexchange-neotree-project-dir-toggle <https://emacs.stackexchange.com/a/29500>`__
   Toggle neotree in the project root.
`stackexchange-revert-all-buffers <https://emacs.stackexchange.com/a/50730>`__
   Revert all buffers, with progress output.
`stackexchange-scratch-buffer-from-file <https://emacs.stackexchange.com/a/38709>`__
   Use a file for the scratch buffer on startup.
`stackexchange-shell-command-on-region-and-select <https://emacs.stackexchange.com/a/34900>`__
   Run command on region, keeping the selection.
`stackexchange-sort-line-in-block <https://emacs.stackexchange.com/a/54023>`__
   Sort a single line within a block.
`stackexchange-transpose-words <https://emacs.stackexchange.com/a/54055>`__
   Transpose words, keeping the cursor location.

.. END PACKAGE LIST


Updating Packages
=================

Currently you will need to manually update the git repository with a ``git pull``
this could be exosed in Emacs in the future.


Updating from StackExchange
===========================

An automated script is used to extract the script from stackexchange,
so scripts can be updated from their source URL.

This can be done by running:

.. code-block:: sh

   python3 _maintanance/update_from_stackexchange.py


Contributing
============

For contributions, open a new issue with a link to the stack-exchange code-snippet you wish to include,
as well as a brief explanation of what it does and why it's useful.


TODO
====

- Generate auto-loads.
- Byte code-compile elisp files.
- Expose a way to update these packages from Emacs.
