// Use system DPI.
user_pref("browser.display.screen_resolution", 0); // Firefox 2
user_pref("layout.css.dpi", 0);                    // Firefox 3

// Clear the download manager on exit.
user_pref("browser.download.manager.retention", 1);

// Don't shrink images to fit.
user_pref("browser.enable_automatic_image_resizing", false);

// Don't try to "fix" URLs by adding www. and .com
user_pref("browser.fixup.alternate.enabled", false);

// Open almost everything in tabs rather than new windows.
user_pref("browser.link.open_external", 3);
user_pref("browser.link.open_newwindow", 3);
user_pref("browser.link.open_newwindow.restriction", 2);

// Use blank homepage.
user_pref("browser.startup.page", 0);

// Don't warn about about:config.
user_pref("general.warnOnAboutConfig", false);

// Don't load a URL from the clipboard when I middle click on content.
user_pref("middlemouse.contentLoadURL", false);

// Don't suggest downloading plugins.
user_pref("plugin.default_plugin_disabled", false);

// Don't show a message when blocking a popup.
user_pref("privacy.popups.showBrowserMessage", false);