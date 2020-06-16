// Don't try to "fix" URLs by adding www. and .com
user_pref("browser.fixup.alternate.enabled", false);

// Don't remember form entries
user_pref("browser.formfill.enable", false);

// Use blank homepage.
user_pref("browser.startup.page", 0);

// Don't remember zoom levels.
user_pref("browser.zoom.siteSpecific", false);

// Don't show a message when blocking a popup.
user_pref("privacy.popups.showBrowserMessage", false);

// Adblock Plus
user_pref("extensions.adblockplus.patternsbackups", 0);
user_pref("extensions.adblockplus.savestats", false);
