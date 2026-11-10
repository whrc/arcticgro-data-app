// embed.js - lets the website show the explorer in an iframe without a
// second scrollbar.
//  * Adding ?embed=1 to the app address hides the app title (the website has its own).
//  * Inside an iframe, the app tells the parent page its height whenever it
//    changes, and asks it to scroll to the top of the app when the tab changes.
//    The matching snippet for the website is in README.md.
(function () {
  var html = document.documentElement;
  if (/[?&]embed=(1|true)(&|$)/.test(window.location.search)) html.classList.add("embedded");

  var inFrame = false;
  try { inFrame = window.self !== window.top; } catch (e) { inFrame = true; }
  if (!inFrame) return;
  html.classList.add("in-frame");

  var lastHeight = 0, timer = null;
  function send(msg) { window.parent.postMessage(msg, "*"); }

  function measure() {
    var body = document.body;
    if (!body) return;
    var h = Math.ceil(body.getBoundingClientRect().height);
    // content that sticks out of the body (e.g. open dropdowns) also counts
    var kids = body.children;
    for (var i = 0; i < kids.length; i++) {
      var r = kids[i].getBoundingClientRect();
      if (r.height > 0) h = Math.max(h, Math.ceil(r.bottom + window.scrollY));
    }
    if (Math.abs(h - lastHeight) > 2) {
      lastHeight = h;
      send({ type: "arcticgro-discharge-height", height: h });
    }
  }
  function schedule() { clearTimeout(timer); timer = setTimeout(measure, 100); }

  document.addEventListener("DOMContentLoaded", function () {
    if (window.ResizeObserver) new ResizeObserver(schedule).observe(document.body);
    schedule();
  });
  window.addEventListener("load", schedule);
  window.addEventListener("resize", schedule);
  // Shiny's own events are jQuery events
  document.addEventListener("DOMContentLoaded", function () {
    if (window.jQuery) window.jQuery(document).on("shiny:value shiny:idle", schedule);
  });
  document.addEventListener("shown.bs.tab", function () {
    schedule();
    send({ type: "arcticgro-discharge-top" });
  });
  document.addEventListener("shown.bs.collapse", schedule);
  document.addEventListener("hidden.bs.collapse", schedule);
})();

// Hide the loading message once Shiny has drawn the first outputs.
(function () {
  var hidden = false;
  function done() {
    if (hidden) return;
    hidden = true;
    var el = document.getElementById("app-loading");
    if (el) { el.classList.add("done"); setTimeout(function () { el.remove(); }, 400); }
  }
  document.addEventListener("DOMContentLoaded", function () {
    if (window.jQuery) window.jQuery(document).one("shiny:idle", function () { setTimeout(done, 300); });
  });
  setTimeout(done, 20000);  // never leave it up for good
})();
