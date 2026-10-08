// The DAG's chrome on the blockr.ui design system, for what CSS cannot reach.
//
// - The toolbar's icon buttons name themselves with blockr.ui's tooltip
//   rather than the browser's native `title` box. G6 renders the toolbar with
//   `title` attributes, so each one moves to `data-blockr-tooltip`.
// - The canvas cannot read CSS variables, so R/g6r.R writes the tokens' light
//   values. Here the graph takes the tokens' current values instead, on start
//   and whenever the scheme changes (`data-bs-theme` on <html>, the attribute
//   the tokens' dark file keys off). The colours in an element's data, such
//   as a node's status dot, are resolved as G6 draws it.
(function () {
  const ATTR = 'data-blockr-tooltip';

  // A token as a colour the canvas can paint: resolved by the browser (it may
  // be a color-mix()), then flattened onto bg-surface through a 1px canvas,
  // since a translucent tint has to look as it does on the surface. A token
  // also comes as a `var()` with its light value as the fallback, the form the
  // colours in the graph's data take.
  const probe = document.createElement('span');
  const pixel = document.createElement('canvas');
  pixel.width = pixel.height = 1;
  const ctx = pixel.getContext('2d', { willReadFrequently: true });

  function resolve(token, over) {
    probe.style.color = token.startsWith('var(') ? token : `var(${token})`;
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

  // The colours an element carries in its data, such as a node's status dot
  // and collapse button, which R/g6r.R writes as `var()`s. G6 asks for them
  // each time it draws the element, so each is resolved once and kept until
  // the scheme changes.
  let inks = new Map();

  function ink(value) {
    if (typeof value === 'string') {
      if (!value.startsWith('var(')) return value;
      if (!inks.has(value)) inks.set(value, resolve(value));
      return inks.get(value);
    }
    if (Array.isArray(value)) return value.map(ink);
    if (value !== null && typeof value === 'object') {
      return Object.fromEntries(
        Object.entries(value).map(([key, x]) => [key, ink(x)])
      );
    }
    return value;
  }

  window.blockrDag = Object.assign(window.blockrDag || {}, { ink });

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

  // The tool's magnifier, as a symbol the toolbar's `<use>` can point at:
  // G6's icon font draws it in another hand than the other tools.
  function addSymbols() {
    if (document.getElementById('blockr-dag-symbols')) return;
    const sprite = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    sprite.id = 'blockr-dag-symbols';
    sprite.setAttribute('aria-hidden', 'true');
    sprite.style.cssText = 'position:absolute;width:0;height:0;overflow:hidden';
    sprite.innerHTML =
      '<symbol id="blockr-search" viewBox="-1 -1 18 18">' +
      '<path d="M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h-.001c.03.04.062.078' +
      '.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1.007 1.007 0 0 0' +
      '-.115-.1zM12 6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0z"/></symbol>';
    document.body.appendChild(sprite);
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
    inks = new Map();
    document.querySelectorAll('.dag-canvas-container').forEach(paint);
  }).observe(document.documentElement, {
    attributes: true,
    attributeFilter: ['data-bs-theme']
  });

  $(document).on('shiny:value shiny:bound', scan);
  $(addSymbols);
  $(scan);
})();
