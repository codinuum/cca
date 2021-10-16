// Modules to control application life and create native browser window
const {app, BrowserWindow, ipcMain, dialog} = require('electron')
const path = require('path')
const crypto = require('crypto')
const fs = require('fs')
const parseArgs = require('minimist')

function getMD5(path) {
  const md5hash = crypto.createHash('md5');
  md5hash.update(fs.readFileSync(path));
  return md5hash.digest('hex');
}

function createWindow () {
  // Create the browser window.
  const mainWindow = new BrowserWindow({
    width: 1280,
    height: 800,
    webPreferences: {
      nodeIntegration: false,
      enableRemoteModule: true,
      contextIsolation: true,
      preload: path.join(__dirname, 'preload.js')
    }
  });
  // and load the index.html of the app.
  mainWindow.loadFile('index.html');
  // Open the DevTools.
  //mainWindow.webContents.openDevTools({mode: 'bottom'})
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.whenReady().then(() => {

  createWindow();

  app.on('activate', function () {
    // On macOS it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if (BrowserWindow.getAllWindows().length === 0) createWindow()
  })
})

// Quit when all windows are closed, except on macOS. There, it's common
// for applications and their menu bar to stay active until the user quits
// explicitly with Cmd + Q.
app.on('window-all-closed', function () {
  //if (process.platform !== 'darwin')
    app.quit();
})

const filters = [
  { name : 'Java', extensions : [ 'java' ] },
  { name : 'C/C++', extensions : [ 'c', 'h', 'C', 'H', 'cpp', 'hpp', 'cc', 'hh' ] },
  { name : 'Fortran', extensions : [ 'f', 'F', 'for', 'FOR', 'f90', 'F90', 'h90', 'H90',
                                     'f95', 'F95', 'f03', 'F03', 'f08', 'F08' ] },
  { name : 'Python', extensions : [ 'py' ] },
  { name : 'Verilog', extensions : [ 'v' ] },
  { name: 'All Files', extensions: ['*'] }
]

function error(err) {
  console.log("[ERROR] "+err);
}

ipcMain.on('sync-mesg', (event, arg) => {
  const args = parseArgs(process.argv);

  const cache = args.cache
  var file0 = args.file0
  var file1 = args.file1

  if (file0) {
  } else {
    var r0, r1;
    r0 = dialog.showOpenDialogSync(null, {
      properties : ['openFile','showHiddenFiles'],
      title : 'Select the first source file',
      message : 'Select the first source file',
      filters : filters
    });

    if (r0) {
      file0 = r0[0];

      if (file1) {
      } else {
        r1 = dialog.showOpenDialogSync(null, {
          properties : ['openFile','showHiddenFiles'],
          title : 'Select the second source file',
          message : 'Select the second source file',
          filters : filters
        });
        if (r1)
          file1 = r1[0];
      }

    } else {
      app.quit();
    }
  }

  if (file0 && file1) {

    const apath0 = path.resolve(process.cwd(), file0);

    if (fs.existsSync(apath0)) {

      const apath1 = path.resolve(process.cwd(), file1);

      if (fs.existsSync(apath1)) {

        var apathd = null;
        if (cache) {
          apathd = path.resolve(process.cwd(), cache)
        } else {
          apathd = path.join(app.getPath('home'), '.cca', 'cache');
        }
        const hash0 = getMD5(apath0);
        const hash1 = getMD5(apath1);
        apathd = path.join(apathd, hash0.substring(0, 2), hash0+'-'+hash1, 'diff.json')

        if (fs.existsSync(apathd)) {

          global.apath0 = apath0;
          global.apath1 = apath1;
          global.apathd = apathd;

          event.returnValue = { apath0: apath0, apath1: apath1, apathd: apathd };
/*
          console.log("loading files...");

          fs.readFile(apath0, {encoding:'utf-8'}, function (err, original) {
            if (err) {
              error(err);
            } else {
              console.log("original source file loaded.");
              fs.readFile(apath1, {encoding:'utf-8'}, function (err, modified) {
                if (err) {
                  error(err);
                } else {
                  console.log("modified source file loaded.");
                  fs.readFile(apathd, {}, function (err, diff) {
                    if (err) {
                      error(err);
                    } else {
                      console.log("diff file loaded.");

                      event.returnValue = {
                        apath0: apath0,
                        apath1: apath1,
                        original: original,
                        modified: modified,
                        diff: JSON.parse(diff)
                      };

                    }

                  });
                }
              });
            }
          });
*/
        } else {
          dialog.showErrorBox('Error!', 'cache not found: '+apathd);
          app.quit();
        }

      } else {
        dialog.showErrorBox('Error!', 'not found: '+apath1);
        app.quit();
      }

    } else {
      dialog.showErrorBox('Error!', 'not found: '+apath0);
      app.quit();
    }

  } else {
    app.quit();
  }

})

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.

