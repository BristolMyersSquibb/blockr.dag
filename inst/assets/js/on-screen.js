// The cards on screen, for the board's lazy evaluation.
//
// Core evaluates a lazy board's blocks only while an owner holds them eager,
// and renders a block's outputs only once it is reported painted. The canvas
// holds the cards it shows: after the graph renders, draws and pans or zooms,
// it reports the blocks whose card lies in the viewport, widened by a margin
// so a card about to scroll in is ready. A collapsed card is hidden, hence not
// on screen. The report goes to the input the board names on its container,
// only when the set changes.
(function () {
  const MARGIN = 0.25;
  const WAIT_MS = 100;
  const EVENTS = ['afterrender', 'afterdraw', 'aftertransform'];

  function onScreen(graph) {
    const [w, h] = graph.getSize();
    const [x0, y0] = graph.getCanvasByViewport([-w * MARGIN, -h * MARGIN]);
    const [x1, y1] = graph.getCanvasByViewport([w * (1 + MARGIN), h * (1 + MARGIN)]);
    return graph.getNodeData()
      .filter((node) => {
        if (graph.getElementVisibility(node.id) === 'hidden') return false;
        const { min, max } = graph.getElementRenderBounds(node.id);
        return min[0] < x1 && max[0] > x0 && min[1] < y1 && max[1] > y0;
      })
      .map((node) => node.id.replace(/^node-/, ''))
      .sort();
  }

  function watch(board, graph) {
    const input = board.dataset.onScreenInput;
    let last = null;
    let timer = null;
    const report = () => {
      clearTimeout(timer);
      timer = setTimeout(() => {
        if (graph.destroyed) return;
        const ids = onScreen(graph);
        const key = ids.join('\n');
        if (key === last) return;
        last = key;
        Shiny.setInputValue(input, ids);
      }, WAIT_MS);
    };
    EVENTS.forEach((event) => graph.on(event, report));
    window.addEventListener('resize', report);
    report();
  }

  function scan() {
    document.querySelectorAll('.blockr-dag-board[data-on-screen-input]').forEach((board) => {
      if (board.dataset.onScreenWatched) return;
      const widget = board.querySelector('.html-widget');
      const graph = widget && HTMLWidgets.find('#' + widget.id)?.getWidget?.();
      if (!graph || !graph.rendered) return;
      board.dataset.onScreenWatched = '1';
      watch(board, graph);
    });
  }

  $(document).on('shiny:value shiny:idle', scan);
})();
