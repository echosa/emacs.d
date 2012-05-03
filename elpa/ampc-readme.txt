* description
ampc is a controller for the Music Player Daemon (http://mpd.wikia.com/).

** installation
If you use GNU ELPA, install ampc via M-x package-list-packages RET or
(package-install 'ampc).  Otherwise, grab this file and put it somewhere in
your load-path or add the directory the file is in to it, e.g.:

(add-to-list 'load-path "~/.emacs.d/ampc")

Then add one autoload definition:

(autoload 'ampc "ampc" nil t)

Optionally bind a key to this function, e.g.:

(global-set-key (kbd "<f9>") 'ampc)

Byte-compile ampc (M-x byte-compile-file RET /path/to/ampc.el RET) to improve
its performance!

** usage
To invoke ampc, call the command `ampc', e.g. via M-x ampc RET.  Once ampc is
connected to the daemon, it creates its window configuration in the selected
window.  To make ampc use the full frame rather than the selected window,
customise `ampc-use-full-frame'.

ampc offers three independent views which expose different parts of the user
interface.  The current playlist view, the default view at startup, may be
accessed using the `J' (that is `S-j') key.  The playlist view may be
accessed using the `K' key.  The outputs view may be accessed using the `L'
key.

*** current playlist view
The playlist view should look like this

.........................
. 1      . 3  . 4  . 5  .
..........    .    .    .
. 2      .    .    .    .
.        .    .    .    .
.        .    .    .    .
.        ................
.        . 6            .
.        .              .
.........................

Window one exposes basic information about the daemon, such as the current
state (stop/play/pause), the song currently playing, or the volume.

All windows, except the status window, contain a tabular list of items.  Each
item may be selected/marked.  There may be multiple selections.

To mark an entry, move the point to the entry and press `m' (ampc-mark).  To
unmark an entry, press `u' (ampc-unmark).  To unmark all entries, press `U'
(ampc-unmark-all).  To toggle marks, press `t' (ampc-toggle-marks).  To
navigate to the next entry, press `n' (ampc-next-line).  Analogous, pressing
`p' (ampc-previous-line) moves the point to the previous entry.

Window two shows the current playlist.  The song that is currently played by
the daemon, if any, is highlighted.  To delete the selected songs from the
playlist, press `d' (ampc-delete).  To move the selected songs up, press
`<up>' (ampc-up).  Analogous, press `<down>' (ampc-down) to move the selected
songs down.

Windows three to five are tag browsers.  You use them to narrow the song
database to certain songs.  Think of tag browsers as filters, analogous to
piping `grep' outputs through additional `grep' filters.  The property of the
songs that is filtered is displayed in the header line of the window.

Window six shows the songs that match the filters defined by windows three to
five.  To add the selected song to the playlist, press `a' (ampc-add).  This
key binding works in tag browsers as well.  Calling ampc-add in a tag browser
adds all songs filtered up to the selected browser to the playlist.

The tag browsers of the (default) current playlist view (accessed via `J')
are `Genre' (window 3), `Artist' (window 4) and `Album' (window 5).  The key
`M' may be used to fire up a slightly modified current playlist view.  There
is no difference to the default current playlist view other than that the tag
browsers filter to `Genre' (window 3), `Album' (window 4) and `Artist'
(window 5).  Metaphorically speaking, the order of the `grep' filters defined
by the tag browsers is different.

*** playlist view
The playlist view resembles the current playlist view.  The window, which
exposes the playlist content, is split, though.  The bottom half shows a list
of stored playlists.  The upper half does not expose the current playlist
anymore.  Instead, the content of the selected (stored) playlist is shown.
All commands that used to work in the current playlist view and modify the
current playlist now modify the selected (stored) playlist.  The list of
stored playlists is the only view in ampc that may have only one marked
entry.

Again, the key `<' may be used to setup a playlist view with a different
order of tag browsers.

*** outputs view
The outputs view contains a single list which shows the configured outputs of
mpd.  To toggle the enabled property of the selected outputs, press `a'
(ampc-toggle-output-enabled).

*** global keys
Aside from `J', `M', `K', `<' and `L', which may be used to select different
views, ampc defines the following global keys, which may be used in every
window associated with ampc:

`k' (ampc-toggle-play): Toggle play state.  If mpd does not play a song
already, start playing the song at point if the current buffer is the
playlist buffer, otherwise start at the beginning of the playlist.  With
prefix argument 4, stop player rather than pause if applicable.

`l' (ampc-next): Play next song.
`j' (ampc-previous): Play previous song

`c' (ampc-clear): Clear playlist.
`s' (ampc-shuffle): Shuffle playlist.

`S' (ampc-store): Store playlist.
`O' (ampc-load): Load selected playlist in the current playlist.
`R' (ampc-rename-playlist): Rename selected playlist.
`D' (ampc-delete-playlist): Delete selected playlist.

`y' (ampc-increase-volume): Increase volume.
`M-y' (ampc-decrease-volume): Decrease volume.
`h' (ampc-increase-crossfade): Increase crossfade.
`M-h' (ampc-decrease-crossfade): Decrease crossfade.

`e' (ampc-toggle-repeat): Toggle repeat state.
`r' (ampc-toggle-random): Toggle random state.
`f' (ampc-toggle-consume): Toggle consume state.

`P' (ampc-goto-current-song): Select the current playlist window and move
point to the current song.

`T' (ampc-trigger-update): Trigger a database update.
`Z' (ampc-suspend): Suspend ampc.
`q' (ampc-quit): Quit ampc.

The keymap of ampc is designed to fit the QWERTY United States keyboard
layout.  If you use another keyboard layout, feel free to modify
ampc-mode-map.  For example, I use a regular QWERTZ German keyboard (layout),
so I modify `ampc-mode-map' in my init.el like this:

(eval-after-load 'ampc
  '(flet ((substitute-ampc-key
           (from to)
           (define-key ampc-mode-map to (lookup-key ampc-mode-map from))
           (define-key ampc-mode-map from nil)))
     (substitute-ampc-key (kbd "z") (kbd "Z"))
     (substitute-ampc-key (kbd "y") (kbd "z"))
     (substitute-ampc-key (kbd "M-y") (kbd "M-z"))
     (substitute-ampc-key (kbd "<") (kbd ";"))))

If ampc is suspended, you can still use every interactive command that does
not directly operate on or with the user interace of ampc.  For example it is
perfectly fine to call `ampc-increase-volume' or `ampc-toggle-play' via M-x
RET.  To display the information that is displayed by the status window of
ampc, call `ampc-status'.
