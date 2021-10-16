// All of the Node.js APIs are available in the preload process.
// It has the same sandbox as a Chrome extension.

const {remote,contextBridge} = require('electron');
const {ipcRenderer} = require('electron');

const sendSync = (mesg, req) => {
  return ipcRenderer.sendSync(mesg, req);
}

contextBridge.exposeInMainWorld('dvapi', {
    sendSync: sendSync,
  }
)
