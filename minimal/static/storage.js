// Initialize the database and return the connection.
// The storeNames argument is an array of store names.
// The function returns the initialized database connection.
export async function init({
  dbName = "Storage",
  version = 1,
  storeNames = [],
}) {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open(dbName, version);
    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);
    request.onupgradeneeded = (event) => {
      const db = event.target.result;
      for (const storeName of storeNames) {
        if (!db.objectStoreNames.contains(storeName)) {
          db.createObjectStore(storeName, { keyPath: "key" });
        }
      }
    };
  });
}

export async function write({ db, storeName, key, data }) {
  const transaction = db.transaction([storeName], "readwrite");
  const store = transaction.objectStore(storeName);
  return new Promise((resolve, reject) => {
    const request = store.put({ key, data });
    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);
  });
}

export async function read({ db, storeName, key }) {
  const transaction = db.transaction([storeName], "readonly");
  const store = transaction.objectStore(storeName);
  return new Promise((resolve, reject) => {
    const request = store.get(key);
    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result?.data);
  });
}
