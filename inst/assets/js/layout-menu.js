// The toolbar's layout menu, and what a layout changes besides positions.
//
// - The menu sets the layered layout's direction and spacing (R's
//   `dag_layout()`), as segmented controls. A change applies at once, in the
//   browser, and is reported as `input$layout` so it persists; the server
//   sends a layout set from outside the same way back.
// - The flow: running left to right puts a block's ports on its sides and
//   draws links as horizontal curves. Ports come from R in the top-to-bottom
//   form; a node is turned before it is drawn.
(function () {
  const DIRECTIONS = [
    { value: 'TB', label: 'Down' },
    { value: 'LR', label: 'Right' }
  ];
  const SPACINGS = [
    { value: 'compact', label: 'Compact' },
    { value: 'normal', label: 'Normal' },
    { value: 'loose', label: 'Loose' }
  ];

  // Per graph (output id): the input to report to, the g6R layout per
  // direction and spacing, and the current layout.
  const boards = new Map();

  const graphOf = (id) => {
    const w = window.HTMLWidgets && HTMLWidgets.find('#' + id);
    const graph = w && w.getWidget && w.getWidget();
    return graph && !graph.destroyed ? graph : null;
  };

  // --- Flow ---------------------------------------------------------------

  const flowOf = (layout) => (layout && layout.rankdir === 'LR' ? 'LR' : 'TB');

  // A port's placement for a flow, from its top-to-bottom placement.
  const placeFor = (tb, flow) => {
    if (flow === 'TB') return tb;
    if (tb === 'top') return 'left';
    if (tb === 'bottom' || tb === 'label-bottom') return 'right';
    if (Array.isArray(tb) && tb.length === 2) return [0, tb[0]];
    return tb;
  };

  const samePlace = (a, b) => JSON.stringify(a) === JSON.stringify(b);

  // Nodes whose ports don't match the flow yet, turned. The top-to-bottom
  // placement is kept on the port, so turning back is exact.
  const turnedNodes = (graph, flow) => {
    const out = [];
    graph.getNodeData().forEach((node) => {
      const ports = node.style && node.style.ports;
      if (!Array.isArray(ports) || !ports.length) return;
      let changed = false;
      const next = ports.map((p) => {
        const tb = p.tbPlacement === undefined ? p.placement : p.tbPlacement;
        const placement = placeFor(tb, flow);
        if (p.tbPlacement !== undefined && samePlace(placement, p.placement)) return p;
        changed = true;
        return Object.assign({}, p, { tbPlacement: tb, placement });
      });
      if (changed) out.push({ id: node.id, style: Object.assign({}, node.style, { ports: next }) });
    });
    return out;
  };

  // Make the drawing follow the current layout's flow: ports and link type.
  const followFlow = (graph) => {
    const flow = flowOf(graph.getOptions().layout);
    const nodes = turnedNodes(graph, flow);
    if (nodes.length) graph.updateNodeData(nodes);
    const edge = graph.getOptions().edge || {};
    const type = flow === 'LR' ? 'cubic-horizontal' : 'cubic-vertical';
    if (edge.type !== type) graph.setEdge(Object.assign({}, edge, { type }));
    return nodes.length > 0;
  };

  // --- Applying a layout ----------------------------------------------------

  // Into view at most at 100%, so blocks never grow past their size: back to
  // 100% and centred when the drawing fits there, zoomed out to fit when it
  // overflows the canvas either way. (G6's `fitView({ when: 'overflow' })`
  // only zooms out when it overflows both ways.)
  const fitWithoutZoomingIn = async (graph) => {
    await graph.zoomTo(1);
    const [w, h] = graph.getSize();
    const { min, max } = graph.getCanvas().getBounds();
    const [x0, y0] = graph.getViewportByCanvas(min);
    const [x1, y1] = graph.getViewportByCanvas(max);
    if (x1 - x0 > w || y1 - y0 > h) {
      await graph.fitView();
    } else {
      await graph.fitCenter();
    }
  };

  const apply = async (id, spec, { report = true } = {}) => {
    const board = boards.get(id);
    const graph = graphOf(id);
    if (!board || !graph) return;

    const base = board.catalog[spec.direction] && board.catalog[spec.direction][spec.spacing];
    if (!base) return;
    const config = Object.assign({}, base);

    graph.setLayout(config);
    if (followFlow(graph)) await graph.draw();
    await graph.layout();

    // g6R puts the last good layout back when this one fails, and says so
    // through `layout_fallback`; then the change did not take.
    if (graph.getOptions().layout !== config) return;

    board.spec = spec;
    await fitWithoutZoomingIn(graph);
    if (report && window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(board.input, spec);
    }
  };

  // --- The menu -------------------------------------------------------------

  let open = null;

  const close = () => {
    if (!open) return;
    const { panel, layer, placed } = open;
    open = null;
    layer.remove();
    placed.stop();
    panel.remove();
  };

  const control = (label, options, value, onChange) => {
    const wrap = document.createElement('div');
    wrap.className = 'dag-layout-menu__control';
    const name = document.createElement('span');
    name.className = 'dag-layout-menu__control-label';
    name.textContent = label;
    wrap.appendChild(name);
    wrap.appendChild(Blockr.segmented(options, value, onChange, { size: 'xs', label }).el);
    return wrap;
  };

  const render = (panel, id) => {
    const board = boards.get(id);
    panel.textContent = '';

    const cap = document.createElement('div');
    cap.className = 'blockr-menu__caption';
    cap.textContent = 'Layout';
    panel.appendChild(cap);

    const controls = document.createElement('div');
    controls.className = 'dag-layout-menu__controls';
    controls.appendChild(control(
      'Direction', DIRECTIONS, board.spec.direction,
      (direction) => apply(id, Object.assign({}, boards.get(id).spec, { direction }))
    ));
    controls.appendChild(control(
      'Spacing', SPACINGS, board.spec.spacing,
      (spacing) => apply(id, Object.assign({}, boards.get(id).spec, { spacing }))
    ));
    panel.appendChild(controls);

    const divider = document.createElement('div');
    divider.className = 'blockr-menu__divider';
    divider.setAttribute('role', 'separator');
    panel.appendChild(divider);

    const list = document.createElement('div');
    list.className = 'blockr-menu__list';
    list.setAttribute('role', 'menu');
    const rearrange = document.createElement('button');
    rearrange.type = 'button';
    rearrange.className = 'blockr-menu__item blockr-menu__item--quiet';
    rearrange.setAttribute('role', 'menuitem');
    const text = document.createElement('span');
    text.className = 'blockr-menu__label';
    text.textContent = 'Re-arrange blocks';
    rearrange.appendChild(text);
    rearrange.addEventListener('click', async () => {
      close();
      await apply(id, boards.get(id).spec, { report: false });
    });
    list.appendChild(rearrange);
    panel.appendChild(list);
  };

  const toggleMenu = (graph, anchor) => {
    const id = graph.options.container;
    if (open) {
      const same = open.id === id;
      close();
      if (same) return;
    }
    if (!boards.has(id)) return;

    const panel = document.createElement('div');
    panel.className = 'blockr-menu dag-layout-menu';
    render(panel, id);
    document.body.appendChild(panel);

    const placed = Blockr.place(panel, anchor, { width: { min: 220, max: 280 } });
    const layer = Blockr.layer([panel], { from: anchor, escape: close, outside: close });
    open = { id, panel, layer, placed };
  };

  // --- Wiring ---------------------------------------------------------------

  const hooked = new WeakSet();

  // Nodes added later arrive with top-to-bottom ports.
  const hook = (graph) => {
    if (hooked.has(graph)) return;
    hooked.add(graph);
    graph.on('beforedraw', () => {
      const flow = flowOf(graph.getOptions().layout);
      if (flow === 'TB') return;
      const nodes = turnedNodes(graph, flow);
      if (nodes.length) graph.updateNodeData(nodes);
    });
  };

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    // The g6R layouts, the input to report to and the layout the board
    // starts with, once the graph exists.
    Shiny.addCustomMessageHandler('blockr-dag-layout-init', (m) => {
      boards.set(m.id, { input: m.input, catalog: m.catalog, spec: m.layout });
      const tryHook = (n) => {
        const graph = graphOf(m.id);
        if (graph && graph.rendered) {
          hook(graph);
          if (followFlow(graph)) graph.draw();
          return;
        }
        if (n > 0) setTimeout(() => tryHook(n - 1), 100);
      };
      tryHook(100);
    });

    // A layout set from outside (the board update lifecycle).
    Shiny.addCustomMessageHandler('blockr-dag-layout', (m) => {
      apply(m.id, m.layout, { report: true });
    });
  }

  window.blockrDag = Object.assign(window.blockrDag || {}, {
    layoutMenu: toggleMenu,
    flowOf,
    placeFor
  });
})();
