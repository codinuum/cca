// Modules to control application life and create native browser window
const {app, BrowserWindow, ipcMain, dialog} = require('electron')
const path = require('path')
const crypto = require('crypto')
const fs = require('fs')
const git = require('isomorphic-git')
const parseArgs = require('minimist')

function getMD5(path) {
  const md5hash = crypto.createHash('md5');
  md5hash.update(fs.readFileSync(path));
  return md5hash.digest('hex');
}

async function getGitObject(pathr, oid) {
  let {blob} = await git.readBlob({
    fs,
    dir: pathr,
    oid: oid
  });
  return Buffer.from(blob).toString('utf8');
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

  const cache = args.cache;
  const local_cache_name = args.localcachename;
  const git_repo_dir = args.git;

  if (git_repo_dir) {
    const apathr = path.resolve(process.cwd(), git_repo_dir);

    if (fs.existsSync(apathr)) {
      const path1 = args.path;
      var path0 = args.path0;
      if (path0) {
      } else {
        path0 = path1;
      }
      const oid0 = args.oid0;
      const oid1 = args.oid1;

      if (path1 && oid0 && oid1) {

        var apathd = null;
        if (cache) {
          apathd = path.resolve(process.cwd(), cache)
        } else {
          apathd = path.join(app.getPath('home'), '.cca', 'cache');
        }
        _apathd = path.join(apathd, oid0.substring(0, 2), oid0+'-'+oid1);
        if (local_cache_name) {
          _apathd = path.join(_apathd, local_cache_name);
        }
        apathd = path.join(_apathd, 'diff.json');

        if (!fs.existsSync(apathd) && !local_cache_name) {
          const dents = fs.readdirSync(_apathd, {withFileTypes: true});
          for (var i = 0; i < dents.length; i++) {
            const dent = dents[i];
            if (dent.isDirectory()) {
              const dp = path.join(_apathd, dent.name, 'diff.json');
              if (fs.existsSync(dp)) {
                apathd = dp;
                break;
              }
            }
          }
        }

        if (fs.existsSync(apathd)) {

          getGitObject(apathr, oid0).then((blob0) => {
            getGitObject(apathr, oid1).then((blob1) => {
              global.apath0 = path0;
              global.apath1 = path1;
              global.apathd = apathd;
              global.blob0 = blob0;
              global.blob1 = blob1;

              event.returnValue = {
                apath0: path0,
                apath1: path1,
                apathd: apathd,
                blob0: blob0,
                blob1: blob1
              };

            });
          });

        } else {
          dialog.showErrorBox('Error!', 'cache not found: '+apathd);
          app.quit();
        }

      } else {
        dialog.showErrorBox('Error!', 'not found: path1, oid0, or oid1');
        app.quit();
      }

    } else {
      dialog.showErrorBox('Error!', 'repository not found: '+apathr);
      app.quit();
    }

  } else {
    var file0 = args.file0;
    var file1 = args.file1;

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
          _apathd = path.join(apathd, hash0.substring(0, 2), hash0+'-'+hash1);
          if (local_cache_name) {
            _apathd = path.join(_apathd, local_cache_name);
          }
          apathd = path.join(_apathd, 'diff.json');

          if (!fs.existsSync(apathd) && !local_cache_name) {
            const dents = fs.readdirSync(_apathd, {withFileTypes: true});
            for (var i = 0; i < dents.length; i++) {
              const dent = dents[i];
              if (dent.isDirectory()) {
                const dp = path.join(_apathd, dent.name, 'diff.json');
                if (fs.existsSync(dp)) {
                  apathd = dp;
                  break;
                }
              }
            }
          }

          if (fs.existsSync(apathd)) {

            global.apath0 = apath0;
            global.apath1 = apath1;
            global.apathd = apathd;

            event.returnValue = { apath0: apath0, apath1: apath1, apathd: apathd };

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

  }

})

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.

