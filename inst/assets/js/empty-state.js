$(function () {
  Shiny.addCustomMessageHandler('update-empty-state', (m) => {
    const el = document.getElementById(m.id);
    if (el) {
      el.style.display = m.show ? '' : 'none';
    }
  });
});
