// Context Menu Enhancement for blockr.dag
// This script adds styling to primary G6 context menu items

// Function to style primary action menu items
function stylePrimaryMenuItems() {
  // Look for all possible context menu containers
  const menus = document.querySelectorAll('[class*="context-menu"], [class*="contextmenu"]');

  menus.forEach(menu => {
    // Find all menu items (try different possible selectors)
    const items = menu.querySelectorAll('li, [class*="item"], [role="menuitem"]');

    items.forEach(item => {
      const text = item.textContent || '';

      // Check if this is a primary action (Append block or Add block)
      // Note: G6 context menu items don't have data-value attributes, so we match by text
      if (text.includes('Append block') || text.includes('Add block')) {

        // Apply subtle background with separator
        item.style.cssText = `
          background: #f8f9fa !important;
          color: #212529 !important;
          font-weight: 500 !important;
          padding: 10px 16px !important;
          margin-left: 0 !important;
          margin-right: 0 !important;
          margin-bottom: 8px !important;
          padding-bottom: 12px !important;
          border-bottom: 1px solid #dee2e6 !important;
          transition: background-color 0.15s ease !important;
        `;

        // Add subtle hover effect
        item.addEventListener('mouseenter', function() {
          this.style.backgroundColor = '#e9ecef';
        });

        item.addEventListener('mouseleave', function() {
          this.style.backgroundColor = '#f8f9fa';
        });

        // Only add the icon if it's not already there (check for ➕)
        if (!text.includes('➕')) {
          item.innerHTML = '<span style="color: #198754; font-size: 1.3em; margin-right: 8px; vertical-align: middle;">➕</span>' +
                          '<span style="vertical-align: middle;">' + text + '</span>';
        }
      }
    });
  });
}

// Use MutationObserver to detect when context menus are added to the DOM
const observer = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    mutation.addedNodes.forEach(function(node) {
      if (node.nodeType === 1) { // Element node
        // Check if this node or its children contain a context menu
        if (node.matches && (node.matches('[class*="context-menu"]') ||
            node.matches('[class*="contextmenu"]'))) {
          setTimeout(stylePrimaryMenuItems, 10);
        } else if (node.querySelector) {
          const hasMenu = node.querySelector('[class*="context-menu"], [class*="contextmenu"]');
          if (hasMenu) {
            setTimeout(stylePrimaryMenuItems, 10);
          }
        }
      }
    });
  });
});

// Start observing the document for DOM changes
if (document.body) {
  observer.observe(document.body, {
    childList: true,
    subtree: true
  });
} else {
  document.addEventListener('DOMContentLoaded', function() {
    observer.observe(document.body, {
      childList: true,
      subtree: true
    });
  });
}
