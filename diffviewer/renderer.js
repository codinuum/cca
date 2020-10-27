// This file is required by the index.html file and will
// be executed in the renderer process for that window.
// No Node.js APIs are available in this process because
// `nodeIntegration` is turned off. Use `preload.js` to
// selectively enable features needed in the rendering
// process.

const {ipcRenderer} = window.native;
const {apath0, apath1, apathd} = ipcRenderer.sendSync('sync-mesg', 'req')
//const {apath0, apath1, original, modified, diff} = ipcRenderer.sendSync('sync-mesg', 'req')

var dv = new DiffViewer();

window.addEventListener('DOMContentLoaded', () => {
  const replaceText = (selector, text) => {
    const element = document.getElementById(selector)
    if (element) element.innerText = text
  }
  replaceText('path0', apath0);
  replaceText('path1', apath1);
})

$(document).ready(function(){
  var url0 = 'file://'+apath0;
  var url1 = 'file://'+apath1;
  var urld = 'file://'+apathd;
  var lang = getLang(apath0);
  var view = document.getElementById("view");

  document.getElementById('close-button').addEventListener('click', () => {
    window.close();
  });
  document.getElementById('diff-button').addEventListener('click', () => {
    dv.toggleDifferences();
  });
  document.getElementById('ast-diff-button').addEventListener('click', () => {
    dv.toggleDts();
  });

  dv.dts = 1;
  dv.show(url0, url1, urld, view, lang, 0, 0, [], [], []);
  //dv.show2(original, modified, diff, view, lang, 0, 0, [], [], []);
})
