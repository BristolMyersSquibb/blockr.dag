// Room for a card that grew.
//
// A card's node takes its card's height (g6R's autoHeight), which changes
// after the layout ran: as the card's outputs render, and as its sections
// open. g6R keeps the card's top edge and announces the new height with
// `g6:node-resize`. A card that grew then reaches into the cards below it, so
// each card it now overlaps moves down until the layout's gap is back above
// it, and the cards below those follow. A card that shrank moves nothing: the
// cards below keep their place, and growing back fills the room it left
// rather than pushing again. The moves are drawn, so they reach the board's
// positions as a drag does.
//
// Room is made once a card's height has held for a moment, from the height it
// settled at: a card can pass through a taller height on its way, such as at
// the end of a section's opening, and pushing for that would leave a gap.
(function () {
  const SETTLE_MS = 150;
  const pending = new WeakMap();

  const nodeBox = (graph, id) => {
    const [x, y] = graph.getElementPosition(id);
    const size = graph.getNodeData(id).style?.size;
    const [w, h] = Array.isArray(size) ? size : [size, size];
    return { x, y, w, h, top: y - h / 2, bottom: y + h / 2, left: x - w / 2, right: x + w / 2 };
  };

  function makeRoom(graph, id, gap) {
    const boxes = new Map();
    for (const node of graph.getNodeData()) {
      if (graph.getElementVisibility(node.id) === 'hidden') continue;
      boxes.set(node.id, nodeBox(graph, node.id));
    }
    if (!boxes.has(id)) return;

    const moved = new Set();
    const queue = [id];
    while (queue.length) {
      const above = boxes.get(queue.shift());
      for (const [other, box] of boxes) {
        if (box === above) continue;
        // below it (by centre) and across from it
        if (box.y <= above.y || box.left >= above.right || box.right <= above.left) continue;
        const dy = above.bottom + gap - box.top;
        if (dy <= 0) continue;
        box.y += dy;
        box.top += dy;
        box.bottom += dy;
        moved.add(other);
        queue.push(other);
      }
    }
    if (!moved.size) return;

    graph.updateNodeData(
      [...moved].map((other) => ({ id: other, style: { y: boxes.get(other).y } }))
    );
    graph.draw();
  }

  // Any resize restarts the wait, a shrink too, which is how a passing peak
  // goes by without making room.
  document.addEventListener('g6:node-resize', (event) => {
    const { id, size, previous } = event.detail;
    const board = event.target.closest('.blockr-dag-board');
    const widget = event.target.closest('.html-widget');
    if (!board || !widget) return;
    const graph = HTMLWidgets.find('#' + widget.id)?.getWidget?.();
    if (!graph || graph.destroyed) return;

    const wait = pending.get(graph) ?? { grown: new Set(), timer: null };
    pending.set(graph, wait);
    if (size[1] > previous[1]) wait.grown.add(id);
    clearTimeout(wait.timer);
    wait.timer = setTimeout(() => {
      const grown = [...wait.grown];
      wait.grown.clear();
      if (graph.destroyed) return;
      const gap = Number(board.dataset.cardGap) || 0;
      grown.forEach((node) => {
        if (graph.hasNode(node)) makeRoom(graph, node, gap);
      });
    }, SETTLE_MS);
  });
})();
