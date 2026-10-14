// Small behaviours for Clusnalyst. Everything works without them.
(function () {
  function reducedMotion() {
    return window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  }

  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("cl-start-tour", function (msg) {
      setTimeout(function () {
        var ids = Object.keys(window.driver || {});
        if (!ids.length) return;
        // Drop steps whose target is hidden on this screen (collapsed sidebar or navbar)
        var d = window.driver[ids[ids.length - 1]];
        var defs = (d.steps || []).map(function (s) { return s.options; }).filter(function (o) {
          var el = o && document.querySelector(o.element);
          return el && el.getBoundingClientRect().width > 0;
        });
        if (!defs.length) return;
        d.defineSteps(defs);
        d.start(0);
      }, msg.delay || 400);
    });
    Shiny.addCustomMessageHandler("cl-scroll-top", function (msg) {
      window.scrollTo({ top: 0, behavior: reducedMotion() ? "auto" : "smooth" });
      // On narrow screens the step list covers the page, so close it after a step is picked
      if (window.innerWidth < 992) {
        var layout = document.querySelector(".cl-analyse");
        if (layout && !layout.classList.contains("sidebar-collapsed")) {
          var toggle = layout.querySelector(":scope > .collapse-toggle");
          if (toggle) toggle.click();
        }
      }
    });
  });

  // Offer the guided tour the first time someone opens the analysis page
  var offered = false;
  $(document).on("shiny:inputchanged", function (e) {
    if (e.name !== "nav" || e.value !== "analyse" || offered) return;
    offered = true;
    var seen = false;
    try { seen = window.localStorage.getItem("clusnalyst-toured") === "1"; } catch (err) { seen = true; }
    if (seen) return;
    try { window.localStorage.setItem("clusnalyst-toured", "1"); } catch (err) {}
    setTimeout(function () { Shiny.setInputValue("cl_first_visit", true, { priority: "event" }); }, 400);
  });
  // driver.js (the tour) listens only for touchstart on touch-capable screens, so mouse
  // clicks on touchscreen laptops do nothing. Handle those clicks here, skipping the
  // click that follows a real tap so the tour never advances twice.
  var lastTouch = 0;
  document.addEventListener("touchstart", function () { lastTouch = Date.now(); }, true);
  document.addEventListener("click", function (e) {
    if (!("ontouchstart" in document.documentElement) || Date.now() - lastTouch < 800) return;
    var ids = Object.keys(window.driver || {});
    if (!ids.length) return;
    var d = window.driver[ids[ids.length - 1]];
    if (!d.isActivated) return;
    if (e.target.closest(".driver-next-btn")) {
      if (d.hasNextStep()) d.moveNext(); else d.reset();
    } else if (e.target.closest(".driver-prev-btn")) {
      if (d.hasPreviousStep()) d.movePrevious();
    } else if (e.target.closest(".driver-close-btn")) {
      d.reset();
    }
  }, true);
})();
