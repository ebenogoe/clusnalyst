// Small behaviours for Clusnalyst. Everything works without them.
(function () {
  function reducedMotion() {
    return window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  }

  $(document).on("shiny:connected", function () {
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
})();
