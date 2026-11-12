(function () {
  function initializeTheme() {
    const selector = document.getElementById("theme_choice");
    const storageKey = "user_theme";
    const choices = ["auto", "flatly", "darkly"];
    const darkPreference = typeof window.matchMedia === "function"
      ? window.matchMedia("(prefers-color-scheme: dark)")
      : null;

    function reportSystemTheme() {
      Shiny.setInputValue("system_theme", darkPreference?.matches ? "darkly" : "flatly");
    }

    let saved;
    try {
      saved = window.localStorage.getItem(storageKey);
    } catch {
      // The theme can still be changed for this session.
    }

    if (choices.includes(saved)) {
      selector.selectize.setValue(saved);
    }
    window.jQuery(selector).on("change", function () {
      if (choices.includes(selector.value)) {
        try {
          window.localStorage.setItem(storageKey, selector.value);
        } catch {
          // The theme can still be changed for this session.
        }
      }
    });

    reportSystemTheme();
    darkPreference?.addEventListener("change", reportSystemTheme);
  }

  const initialized = window.Shiny?.initializedPromise;
  if (initialized) {
    initialized.then(initializeTheme);
  } else {
    window.jQuery(document).one("shiny:sessioninitialized", initializeTheme);
  }
})();
