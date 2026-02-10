// Compiled with esbuild from file runner/index.ts in @andrewMacmurray/elm-concurrent-task at commit ae4668f2 (2026-02-06)
// esbuild --bundle --platform=neutral index.ts --outdir=dist
//
// http/fetch.ts
function http(request) {
  let controller;
  if (request.timeout) {
    controller = new AbortController();
    setTimeout(() => controller?.abort(), request.timeout);
  }
  return fetch(request.url, {
    method: request.method,
    body: request.body || null,
    headers: new Headers(request.headers),
    signal: controller?.signal,
  })
    .then((res) => {
      const headers = {};
      res.headers.forEach((val, key) => {
        headers[key] = val;
      });
      switch (request.expect) {
        case "STRING": {
          return res.text().then((x) => ({
            url: res.url,
            headers,
            statusCode: res.status,
            statusText: res.statusText,
            body: x || "",
          }));
        }
        case "JSON": {
          return res.text().then((x) => ({
            url: res.url,
            headers,
            statusCode: res.status,
            statusText: res.statusText,
            body: x || null,
          }));
        }
        case "BYTES": {
          return res
            .blob()
            .then((blob) => blob.text())
            .then((x) => {
              return {
                url: res.url,
                headers,
                statusCode: res.status,
                statusText: res.statusText,
                body: x || "",
              };
            });
        }
        case "WHATEVER": {
          return {
            url: res.url,
            headers,
            statusCode: res.status,
            statusText: res.statusText,
            body: null,
          };
        }
      }
    })
    .catch((e) => {
      return {
        error: {
          reason: toHttpError(e),
          message: e.message,
        },
      };
    });
}
function toHttpError(err) {
  if (err.name === "AbortError") {
    return "TIMEOUT";
  } else if (err.cause?.code === "ERR_INVALID_URL") {
    return "BAD_URL";
  } else {
    return "NETWORK_ERROR";
  }
}

// browser/dom.ts
function focus(id) {
  return withDomNode(id, (el) => el.focus());
}
function blur(id) {
  return withDomNode(id, (el) => el.blur());
}
function getViewport() {
  return {
    scene: getBrowserScene(),
    viewport: {
      x: window.scrollX,
      y: window.scrollY,
      width: document.documentElement.clientWidth,
      height: document.documentElement.clientHeight,
    },
  };
}
function getViewportOf(id) {
  return withDomNode(id, (el) => ({
    scene: {
      width: el.scrollWidth,
      height: el.scrollHeight,
    },
    viewport: {
      x: el.scrollLeft,
      y: el.scrollTop,
      width: el.clientWidth,
      height: el.clientHeight,
    },
  }));
}
function setViewport(options) {
  window.scroll(options.y, options.y);
}
function setViewportOf(options) {
  return withDomNode(options.id, (el) => {
    el.scrollLeft = options.x;
    el.scrollTop = options.y;
  });
}
function getElement(id) {
  return withDomNode(id, (el) => {
    const rect = el.getBoundingClientRect();
    const x = window.scrollX;
    const y = window.scrollY;
    return {
      scene: getBrowserScene(),
      viewport: {
        x,
        y,
        width: document.documentElement.clientWidth,
        height: document.documentElement.clientHeight,
      },
      element: {
        x: x + rect.left,
        y: y + rect.top,
        width: rect.width,
        height: rect.height,
      },
    };
  });
}
function withDomNode(id, callback) {
  const el = document.getElementById(id);
  if (el) {
    return callback(el);
  }
  return { error: null };
}
function getBrowserScene() {
  const body = document.body;
  const elem = document.documentElement;
  return {
    width: Math.max(
      body.scrollWidth,
      body.offsetWidth,
      elem.scrollWidth,
      elem.offsetWidth,
      elem.clientWidth,
    ),
    height: Math.max(
      body.scrollHeight,
      body.offsetHeight,
      elem.scrollHeight,
      elem.offsetHeight,
      elem.clientHeight,
    ),
  };
}

// index.ts
var BuiltInTasks = {
  debugLog: console.log,
  http,
  timeNow: () => Date.now(),
  timeZoneOffset: () => getTimezoneOffset(),
  timeZoneName: () => getTimeZoneName(),
  randomSeed: () => Date.now(),
  sleep,
  domFocus: focus,
  domBlur: blur,
  domGetViewport: getViewport,
  domGetViewportOf: getViewportOf,
  domSetViewport: setViewport,
  domSetViewportOf: setViewportOf,
  domGetElement: getElement,
};
function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
function getTimezoneOffset() {
  return -(/* @__PURE__ */ new Date().getTimezoneOffset());
}
function getTimeZoneName() {
  try {
    return Intl.DateTimeFormat().resolvedOptions().timeZone;
  } catch (e) {
    return /* @__PURE__ */ new Date().getTimezoneOffset();
  }
}
function register(options) {
  const tasks = createTasks(options);
  const subscribe = options.ports.send.subscribe;
  const send = options.ports.receive.send;
  subscribe(async (payload) => {
    const debouncedSend = debounce(send, debounceThreshold(payload));
    for (const def of payload) {
      if (!tasks[def.function]) {
        return debouncedSend({
          attemptId: def.attemptId,
          taskId: def.taskId,
          result: {
            error: {
              reason: "missing_function",
              message: `${def.function} is not registered`,
            },
          },
        });
      }
    }
    payload.map(async (def) => {
      try {
        logTaskStart(def, options);
        const result = await tasks[def.function]?.(def.args);
        logTaskFinish(def, options);
        debouncedSend({
          attemptId: def.attemptId,
          taskId: def.taskId,
          result: { value: result },
        });
      } catch (e) {
        debouncedSend({
          attemptId: def.attemptId,
          taskId: def.taskId,
          result: {
            error: {
              reason: "js_exception",
              message: `${e.name}: ${e.message}`,
              raw: e,
            },
          },
        });
      }
    });
  });
}
function logTaskStart(def, options) {
  const logStart =
    options.debug &&
    typeof options.debug !== "boolean" &&
    options.debug.taskStart;
  if (logStart || options.debug === true) {
    console.info(
      `--starting-- ${def.function} attempt-${def.attemptId} id-${def.taskId}`,
    );
  }
}
function logTaskFinish(def, options) {
  const logStart =
    options.debug &&
    typeof options.debug !== "boolean" &&
    options.debug.taskFinish;
  if (logStart || options.debug === true) {
    console.info(
      `--complete-- ${def.function} attempt - ${def.attemptId} id - ${def.taskId}`,
    );
  }
}
function debounceThreshold(defs) {
  return defs.length > 10 ? 20 : 0;
}
function debounce(send, wait) {
  let timeout;
  let results = [];
  return function enqueueResult(taskResult) {
    results.push(taskResult);
    const later = () => {
      clearTimeout(timeout);
      send(results);
      results = [];
    };
    clearTimeout(timeout);
    timeout = setTimeout(later, wait);
  };
}
function createTasks(options) {
  const builtins = {
    ...BuiltInTasks,
    ...(options.builtins || {}),
  };
  return {
    ...prefixWith("builtin:", builtins),
    ...options.tasks,
  };
}
function prefixWith(prefix, tasks) {
  return Object.fromEntries(
    Object.entries(tasks).map(([key, fn]) => [`${prefix}${key}`, fn]),
  );
}
export { register };
