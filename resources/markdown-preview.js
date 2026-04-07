// Markdown Preview — foldable headline sections
(function () {
  "use strict";

  function init() {
    var headings = document.querySelectorAll("h1, h2, h3, h4, h5, h6");
    if (!headings.length) return;

    headings.forEach(function (heading) {
      var level = parseInt(heading.tagName[1], 10);

      // Collect sibling nodes until next heading of same or higher level
      var content = [];
      var sibling = heading.nextElementSibling;
      while (sibling) {
        if (/^H[1-6]$/.test(sibling.tagName)) {
          var sibLevel = parseInt(sibling.tagName[1], 10);
          if (sibLevel <= level) break;
        }
        content.push(sibling);
        sibling = sibling.nextElementSibling;
      }

      if (content.length === 0) return;

      // Wrap content in a fold container
      var wrapper = document.createElement("div");
      wrapper.className = "fold-content";
      heading.parentNode.insertBefore(wrapper, content[0]);
      content.forEach(function (el) { wrapper.appendChild(el); });

      // Make heading clickable
      heading.classList.add("fold-toggle");
      heading.title = "Click to fold/unfold";

      heading.addEventListener("click", function () {
        var isCollapsed = wrapper.classList.toggle("collapsed");
        heading.classList.toggle("collapsed", isCollapsed);
        if (!isCollapsed) {
          wrapper.style.maxHeight = wrapper.scrollHeight + "px";
        }
      });

      // Set initial max-height for transition
      wrapper.style.maxHeight = wrapper.scrollHeight + "px";
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
