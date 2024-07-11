const CACHE_NAME = "pwa-test";

var offlineResources = [
  "/pwa",
  "/pwa/index.html",
  "/pwa/page1.html",
  "/pwa/page2.html",
  "/pwa/nested/page3.html",
  "/pwa/icon.png"
];

self.addEventListener("install", (event) => {
  event.waitUntil(
    (async () => {
      const cache = await caches.open(CACHE_NAME);
      const clients = await self.clients.matchAll({includeUncontrolled: true});
      for (const resource of offlineResources) {
        console.log(`fetching ${resource}`);
        
        const response = await fetch(resource, {redirect: "manual"});
        if (!response.ok) {
          const body = await response.text();
          throw new TypeError(`bad fetch response: ${JSON.stringify({
            url: response.url,
            ok: response.ok,
            status: response.status,
            body: body
          })}`);
        }
        await cache.put(resource, response);

        if (response.status === 301) {
          offlineResources.push(response.headers.get("Location"));
        }
        
        for (const client of clients) {
          await client.postMessage({type: "installEvent", value: `saved ${resource}`});
        }
      }
      for (const client of clients) {
        await client.postMessage({type: "installSuccess", value: {}});
      }
    })(),
  );
});

self.addEventListener("fetch", (event) => {
  event.respondWith(
    (async () => {
      const cache = await caches.open(CACHE_NAME);
      const cachedResponse = await cache.match(event.request.url);
      if (cachedResponse) {
        return cachedResponse;
      }
      
      return new Response(`${event.request.url} not found (missing from cache)`, { status: 404 });
    })(),
  );
});
