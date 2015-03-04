###########################################################################
# Author: Jesús Martínez-Barquero Herrada
# Last edited: 26/02/2015
###########################################################################
# Description: Replace Sublime Text 3 Settings and Keymaps with symbolic
#       links to this project files.
###########################################################################
# URL: https://github.com/JesusMtnez/configs/tree/master/Sublime%20Text%203
###########################################################################
# NOTE: Only works if the script it is called from the same folder where
#       the script is located.
###########################################################################
#
#!/bin/bash
# Preferences file
ln -sf "$(pwd)/Settings-Keys/Preferences.sublime-settings" "${HOME}/.config/sublime-text-3/Packages/User/Preferences.sublime-settings";
# Linux Keymap file
ln -sf "$(pwd)/Settings-Keys/Default (Linux).sublime-keymap" "${HOME}/.config/sublime-text-3/Packages/User/Default (Linux).sublime-keymap";
# XML Preferences
ln -sf "$(pwd)/Settings-Keys/XML.sublime-settings" "${HOME}/.config/sublime-text-3/Packages/User/XML.sublime-settings";
###########################################################################
# Plugins
###########################################################################
# Markdown GFM Settings
ln -sf "$(pwd)/Settings-Keys/Markdown.sublime-settings" "${HOME}/.config/sublime-text-3/Packages/User/Markdown.sublime-settings";
# Plain Tasks Settings
ln -sf "$(pwd)/Settings-Keys/PlainTasks.sublime-settings" "${HOME}/.config/sublime-text-3/Packages/User/PlainTasks.sublime-settings";
###########################################################################
# Snippets
###########################################################################
# XML
ln -sf "$(pwd)/Snippets/XML_Header.sublime-snippet" "${HOME}/.config/sublime-text-3/Packages/User/XML_Header.sublime-snippet";
ln -sf "$(pwd)/Snippets/XML_Tag.sublime-snippet" "${HOME}/.config/sublime-text-3/Packages/User/XML_Tag.sublime-snippet";
###########################################################################
# Color Schemes
###########################################################################
# Batman (for XML files)
ln -sf "$(pwd)/ColorSchemes-Themes/Batman.tmTheme" "${HOME}/.config/sublime-text-3/Packages/User/Batman.tmTheme";
# Cobaltish (for Markdown files)
ln -sf "$(pwd)/ColorSchemes-Themes/MarkdownEditor-Cobaltish.tmTheme" "${HOME}/.config/sublime-text-3/Packages/User/MarkdownEditor-Cobaltish.tmTheme";