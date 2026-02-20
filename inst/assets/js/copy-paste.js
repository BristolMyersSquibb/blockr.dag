$(function () {
  Shiny.addCustomMessageHandler('setup-copy-paste', (m) => {
    $(document).on('keydown', async (e) => {
      // Skip if user is typing in an input/textarea/select
      if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA' ||
          e.target.tagName === 'SELECT' || e.target.isContentEditable) {
        return;
      }

      if (e.key === 'c' && (e.ctrlKey || e.metaKey)) {
        e.preventDefault();
        Shiny.setInputValue(
          `${m.id}-copy_selected`,
          true,
          { priority: 'event' }
        );
      }

      if (e.key === 'x' && (e.ctrlKey || e.metaKey)) {
        e.preventDefault();
        Shiny.setInputValue(
          `${m.id}-cut_selected`,
          true,
          { priority: 'event' }
        );
      }

      if (e.key === 'v' && (e.ctrlKey || e.metaKey)) {
        e.preventDefault();
        try {
          const text = await navigator.clipboard.readText();
          const data = JSON.parse(text);
          if (data && data.blocks) {
            Shiny.setInputValue(
              `${m.id}-paste_clipboard`,
              text,
              { priority: 'event' }
            );
          }
        } catch (err) {
          // Silently ignore clipboard read or parse failures
        }
      }
    });
  });

  Shiny.addCustomMessageHandler('write-clipboard', (m) => {
    try {
      navigator.clipboard.writeText(m.json);
    } catch (err) {
      // Silently ignore clipboard write failures
    }
  });
});
