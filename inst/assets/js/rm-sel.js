$(function () {
  Shiny.addCustomMessageHandler('setup-remove-selected-elements', (m) => {
    $(document).on('keydown', (e) => {
      if (e.key == m.key && e.ctrlKey) { // Ctrl + Delete
        e.preventDefault();
        Shiny.setInputValue(`${m.id}-batch_delete`, true, { priority: 'event' });
      }
    });
  });
})
