$(function () {
  // Safari requires clipboard.write() to be called inside a user gesture.
  // The Shiny round-trip (gesture → server → custom message) breaks that
  // chain, so we start the write immediately with a pending Promise and
  // resolve it once the server sends the serialised data back.
  //
  // The shiny:inputchanged event fires synchronously inside setInputValue,
  // so it still runs within the original user gesture. We listen for any
  // copy/cut Shiny input and set up the deferred write centrally — no
  // individual handler needs to know about it.
  let clipboardResolve = null;

  function deferClipboardWrite() {
    const item = new ClipboardItem({
      'text/plain': new Promise((resolve) => {
        clipboardResolve = (text) =>
          resolve(new Blob([text], { type: 'text/plain' }));
      })
    });
    navigator.clipboard.write([item]).catch(() => {});
  }

  $(document).on('shiny:inputchanged', (event) => {
    if (/(copy_selected|cut_selected|ctx_copy|ctx_cut)$/.test(event.name)) {
      deferClipboardWrite();
    }
  });

  Shiny.addCustomMessageHandler('setup-copy-paste', (m) => {
    $(document).on('keydown', async (e) => {
      // Skip if user is typing in an input/textarea/select
      if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA' ||
          e.target.tagName === 'SELECT' || e.target.isContentEditable) {
        return;
      }

      // If the user has text selected anywhere on the page, defer to the
      // browser's default copy/cut so that e.g. column names or error
      // messages land on the clipboard as-is.
      const selection = window.getSelection && window.getSelection();
      const hasTextSelection =
        selection && selection.toString().length > 0;

      if (e.key === 'c' && (e.ctrlKey || e.metaKey)) {
        if (hasTextSelection) return;
        e.preventDefault();
        Shiny.setInputValue(
          `${m.id}-copy_selected`,
          true,
          { priority: 'event' }
        );
      }

      if (e.key === 'x' && (e.ctrlKey || e.metaKey)) {
        if (hasTextSelection) return;
        e.preventDefault();
        Shiny.setInputValue(
          `${m.id}-cut_selected`,
          true,
          { priority: 'event' }
        );
      }

      if (e.key === 'v' && (e.ctrlKey || e.metaKey)) {
        try {
          const text = await navigator.clipboard.readText();
          const data = JSON.parse(text);
          if (data && data.object === 'subboard') {
            e.preventDefault();
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

  Shiny.addCustomMessageHandler('write-clipboard', async (m) => {
    if (clipboardResolve) {
      clipboardResolve(m.json);
      clipboardResolve = null;
    } else {
      try {
        await navigator.clipboard.writeText(m.json);
      } catch (err) {}
    }
  });
});
