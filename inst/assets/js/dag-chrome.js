// The DAG's chrome on the blockr.ui design system, for what CSS cannot reach.
//
// - The toolbar's icon buttons name themselves with blockr.ui's tooltip
//   rather than the browser's native `title` box. G6 renders the toolbar with
//   `title` attributes, so each one moves to `data-blockr-tooltip`.
// - The canvas cannot read CSS variables, so R/g6r.R writes the tokens' light
//   values. Here the graph takes the tokens' current values instead, on start
//   and whenever the scheme changes (`data-bs-theme` on <html>, the attribute
//   the tokens' dark file keys off).
(function () {
  const ATTR = 'data-blockr-tooltip';

  // A token as a colour the canvas can paint: resolved by the browser (it may
  // be a color-mix()), then flattened onto bg-surface through a 1px canvas,
  // since a translucent tint has to look as it does on the surface.
  const probe = document.createElement('span');
  const pixel = document.createElement('canvas');
  pixel.width = pixel.height = 1;
  const ctx = pixel.getContext('2d', { willReadFrequently: true });

  function resolve(token, over) {
    probe.style.color = `var(${token})`;
    document.body.appendChild(probe);
    const value = getComputedStyle(probe).color;
    probe.remove();
    ctx.clearRect(0, 0, 1, 1);
    if (over) {
      ctx.fillStyle = over;
      ctx.fillRect(0, 0, 1, 1);
    }
    ctx.fillStyle = value;
    ctx.fillRect(0, 0, 1, 1);
    const [r, g, b] = ctx.getImageData(0, 0, 1, 1).data;
    return `rgb(${r}, ${g}, ${b})`;
  }

  function palette() {
    const surface = resolve('--blockr-color-bg-surface');
    return {
      surface,
      text: resolve('--blockr-color-text-default', surface),
      muted: resolve('--blockr-color-text-muted', surface),
      strong: resolve('--blockr-color-border-strong', surface),
      accentStrong: resolve('--blockr-color-text-accent-strong', surface),
      selected: resolve('--blockr-color-bg-selected', surface)
    };
  }

  function paint(container) {
    const el = container.querySelector('.html-widget.g6');
    const graph = el && HTMLWidgets.find('#' + el.id)?.getWidget?.();
    if (!graph || graph.destroyed) return;

    const p = palette();
    const o = graph.getOptions();
    const node = o.node || {}, edge = o.edge || {}, combo = o.combo || {};
    const nodeState = node.state || {}, edgeState = edge.state || {};

    graph.setOptions({
      // The ports' ring is drawn in the background colour.
      background: p.surface,
      node: Object.assign({}, node, {
        style: Object.assign({}, node.style, {
          labelFill: p.text,
          labelBackgroundFill: p.surface
        }),
        state: Object.assign({}, nodeState, {
          selected: Object.assign({}, nodeState.selected, {
            labelFill: p.accentStrong,
            labelBackgroundFill: p.selected
          })
        })
      }),
      edge: Object.assign({}, edge, {
        style: Object.assign({}, edge.style, {
          stroke: p.strong,
          labelFill: p.muted,
          labelBackgroundFill: p.surface
        }),
        state: Object.assign({}, edgeState, {
          active: Object.assign({}, edgeState.active, { stroke: p.strong }),
          selected: Object.assign({}, edgeState.selected, { stroke: p.muted })
        })
      }),
      combo: Object.assign({}, combo, {
        style: Object.assign({}, combo.style, { labelFill: p.text })
      })
    });
    graph.draw();
  }

  function adopt(container) {
    container.querySelectorAll('.g6-toolbar-item[title]').forEach((item) => {
      item.setAttribute(ATTR, item.getAttribute('title'));
      item.removeAttribute('title');
    });
    // The toolbar is drawn once the graph is: the first sign it can be painted.
    if (!container.dataset.dagPainted && container.querySelector('.g6-toolbar')) {
      container.dataset.dagPainted = '1';
      paint(container);
    }
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

  new MutationObserver(() => {
    document.querySelectorAll('.dag-canvas-container').forEach(paint);
  }).observe(document.documentElement, {
    attributes: true,
    attributeFilter: ['data-bs-theme']
  });

  $(document).on('shiny:value shiny:bound', scan);
  $(scan);
})();
