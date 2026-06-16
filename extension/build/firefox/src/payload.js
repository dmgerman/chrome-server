// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>
//
// payload.js — build the request payload a menu sends to Emacs.
//
// `payload` in a menu entry is either a STRING (built-in kind) or an
// OBJECT { kind: "...", ... } selecting a parameterised kind.
//
// String kinds (no parameters):
//   page-url                 url, title from the tab (ignores click context)
//   page-url-with-selection  url, title, text (current selection)
//   selection-text           url, title, text (selection or click target)
//   page-html                url, title, html (main/article/body innerHTML)
//   link-url                 url = info.linkUrl, title = selection text
//   image-url                url = info.srcUrl, title = tab title
//   url                      context-aware: link URL > image URL > tab URL.
//                            Use this when a single menu serves multiple
//                            triggers (e.g. "page" + "link") and you want
//                            the click to decide which URL to send.
//
// Object kinds:
//   { kind: "tab-message", method: "...", message: { ... }? }
//       chrome.tabs.sendMessage(tabId, { method, ...message }) -> reply.payload
//       Generic bridge to a domain-specific content script.  The content
//       script registers a runtime.onMessage listener and dispatches on the
//       `method` field, then replies sendResponse({ payload: ... }).  Adding
//       a new scraper is a drop-in: declare the content script in
//       config.contentScripts, dispatch on a fresh `method` in the new
//       script, and reference it from a menu's `payload`.  No JS edit here.
//
// Each handler returns an object suitable as the WS request payload.

async function readSelectionInTab(tabId) {
  try {
    const results = await chrome.scripting.executeScript({
      target: { tabId },
      func: () => window.getSelection?.().toString() ?? "",
    });
    return results?.[0]?.result ?? "";
  } catch (e) {
    return "";
  }
}

async function extractMainHtml(tabId) {
  const results = await chrome.scripting.executeScript({
    target: { tabId },
    func: () => {
      const el = document.querySelector("main") ||
                 document.querySelector("article") ||
                 document.querySelector("[role='main']") ||
                 document.body;
      return { html: el.innerHTML.trim(), url: location.href, title: document.title };
    },
  });
  const r = results?.[0]?.result;
  if (!r) throw new Error("could not extract page content");
  return r;
}

// Invoke METHOD in a tab's content script and resolve with its
// `reply.payload`.  This is the generic bridge used by tab-message payloads.
function callTabMethod(tabId, method, extra) {
  return new Promise((resolve, reject) => {
    const message = { method, ...(extra ?? {}) };
    chrome.tabs.sendMessage(tabId, message, (reply) => {
      if (chrome.runtime.lastError) {
        reject(new Error(
          `tab-message '${method}': ${chrome.runtime.lastError.message} ` +
          `(content script not loaded for this URL?)`
        ));
        return;
      }
      if (!reply?.payload) {
        reject(new Error(`tab-message '${method}': content script returned no payload`));
        return;
      }
      resolve(reply.payload);
    });
  });
}

export async function gatherPayload(payload, { tab, info } = {}) {
  if (!tab) throw new Error("no active tab");

  // Object form: a parameterised kind.
  if (payload && typeof payload === "object") {
    switch (payload.kind) {
      case "tab-message":
        if (!payload.method) {
          throw new Error("tab-message payload requires `method`");
        }
        return await callTabMethod(tab.id, payload.method, payload.message);
      default:
        throw new Error(`unknown payload kind: ${payload.kind}`);
    }
  }

  // String form: a built-in kind.
  switch (payload) {
    case "page-url":
      return { url: tab.url, title: tab.title };

    case "page-url-with-selection":
    case "selection-text": {
      const text = info?.selectionText ?? await readSelectionInTab(tab.id);
      return { url: tab.url, title: tab.title, text };
    }

    case "page-html":
      return await extractMainHtml(tab.id);

    case "link-url":
      return { url: info?.linkUrl ?? tab.url, title: info?.selectionText ?? tab.title };

    case "image-url":
      return { url: info?.srcUrl ?? tab.url, title: tab.title };

    case "url":
      // Context-aware: prefer the most specific URL the click implies.
      return {
        url:   info?.linkUrl ?? info?.srcUrl ?? tab.url,
        title: info?.selectionText || tab.title,
      };

    default:
      throw new Error(`unknown payload kind: ${payload}`);
  }
}
