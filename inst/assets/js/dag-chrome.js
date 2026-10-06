// The toolbar's icon buttons name themselves with blockr.ui's tooltip rather
// than the browser's native `title` box. G6 renders the toolbar with `title`
// attributes, so move each one to `data-blockr-tooltip` as it appears.
(function () {
  const ATTR = 'data-blockr-tooltip';

  function adopt(root) {
    root.querySelectorAll('.g6-toolbar-item[title]').forEach((item) => {
      item.setAttribute(ATTR, item.getAttribute('title'));
      item.removeAttribute('title');
    });
  }

  function watch(container) {
    if (container.dataset.dagChrome) return;
    container.dataset.dagChrome = '1';
    adopt(container);
    new MutationObserver(() => adopt(container)).observe(container, {
      childList: true,
      subtree: true
    });
  }

  function scan() {
    document.querySelectorAll('.dag-canvas-container').forEach(watch);
  }

  $(document).on('shiny:value shiny:bound', scan);
  $(scan);
})();
