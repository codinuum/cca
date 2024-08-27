// This file is required by the index.html file and will
// be executed in the renderer process for that window.
// No Node.js APIs are available in this process because
// `nodeIntegration` is turned off. Use `preload.js` to
// selectively enable features needed in the rendering
// process.

const {apath0, apath1, apathd, blob0, blob1} = window.dvapi.sendSync('sync-mesg', 'req')

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
  const url0 = 'file://'+apath0;
  const url1 = 'file://'+apath1;
  const urld = 'file://'+apathd;
  const lang = getLang(apath0);
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
  dv.show(url0, url1, urld, view, lang, 0, 0, [], [], [], blob0, blob1);
  //dv.show2(original, modified, diff, view, lang, 0, 0, [], [], []);
})
