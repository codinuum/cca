// DiffViewer for Diff/AST, Copyright 2020 Codinuum Software Lab <https://codinuum.com>
// based on
// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

// declare global: diff_match_patch, DIFF_INSERT, DIFF_DELETE, DIFF_EQUAL

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["codemirror", "diff_match_patch"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
  "use strict";
  var Pos = CodeMirror.Pos;
  var svgNS = "http://www.w3.org/2000/svg";

  var classesDts = { del      : "DTS-deleted",
                     ins      : "DTS-inserted",
                     rel      : "DTS-relabeled",
                     mov      : "DTS-moved",
                     movrel   : "DTS-moved-and-relabeled",
                     ign      : "DTS-ignored",
                     mis      : "DTS-missed",
                     frame    : "DTS-frame",
                     frameEss : "DTS-frame-essential",
                     cmov     : "DTS-connector-move",
                     crel     : "DTS-connector-relabel",
                     cmovrel  : "DTS-connector-movrel",
                     calign   : "DTS-connector-align",
                     ccorr    : "DTS-connector-corr",
                     ess      : "DTS-essential-lines",
                     essStart : "DTS-essential-lines-start",
                     essEnd   : "DTS-essential-lines-end",
                   };

  function DiffView(mv, type, index) {
    //console.log("constructing DiffView...");
    this.mv = mv;
    this.type = type;
    this.index = index;
    this.classes = type == "left"
      ? {chunk:  "CodeMirror-merge-l-chunk",
         start:  "CodeMirror-merge-l-chunk-start",
         end:    "CodeMirror-merge-l-chunk-end",
         insert: "CodeMirror-merge-l-inserted",
         del:    "CodeMirror-merge-l-deleted",
         connect: "CodeMirror-merge-l-connect"}
      : {chunk:   "CodeMirror-merge-r-chunk",
         start:   "CodeMirror-merge-r-chunk-start",
         end:     "CodeMirror-merge-r-chunk-end",
         insert:  "CodeMirror-merge-r-inserted",
         del:     "CodeMirror-merge-r-deleted",
         connect: "CodeMirror-merge-r-connect"};
  }

  DiffView.prototype = {
    constructor: DiffView,
    init: function(pane, orig, diffDts, options) {
      this.edit = this.mv.edit;
      (this.edit.state.diffViews || (this.edit.state.diffViews = [])).push(this);
      this.diffDtsEss = this.mv.diffDtsEss;
      var origReadOnly = !this.mv.options.allowEditingOriginals;
      /*
      if (origReadOnly)
        origReadOnly = "nocursor";
      */
      this.orig = CodeMirror(pane, copyObj({value: orig, readOnly: origReadOnly}, copyObj(options)));
      if (this.mv.options.connect == "align") {
        if (!this.edit.state.trackAlignable) this.edit.state.trackAlignable = new TrackAlignable(this.edit)
        this.orig.state.trackAlignable = new TrackAlignable(this.orig)
      }
      this.lockButton.title = this.edit.phrase("Toggle locked scrolling");

      this.orig.state.diffViews = [this];
      var classLocation = options.chunkClassLocation || "background";
      if (Object.prototype.toString.call(classLocation) != "[object Array]") classLocation = [classLocation]
      this.classes.classLocation = classLocation

      this.showDifferences = options.showDifferences !== false;
      this.showDts = options.showDts;

      if (this.showDifferences) {
        this.deferredDiffFlag = false;
        this.diff = getDiff(asString(orig), asString(options.value), this.mv.options.ignoreWhitespace);
        this.chunks = getChunks(this.diff);
      } else { // deferred
        this.deferredDiffFlag = true;
        this.diff = [];
        this.chunks = [];
      }
      this.diffOutOfDate = this.dealigned = false;
      this.needsScrollSync = null

      this.diffDts = diffDts;

      this.showToolTip = true;
      this.toolTipTimer = null;

      this.orig.dv = this;
      this.edit.dv = this;
      this.orig.on("mousedown", handleEditClick);
    },
    registerEvents: function(otherDv) {
      this.forceUpdate = registerUpdate(this);
      setScrollLock(this, false, false);
      registerScroll(this, otherDv);
    },
    setShowDifferences: function(val) {
      val = val !== false;
      if (val != this.showDifferences) {
        if (this.deferredDiffFlag) {
          this.diff = getDiff(this.orig.getValue(), this.edit.getValue(), this.mv.options.ignoreWhitespace);
          this.chunks = getChunks(this.diff);
          this.deferredDiffFlag = false;
        }
        this.showDifferences = val;
        this.forceUpdate("full");
      }
    },
    setShowDts: function(val) {
      if (val != this.showDts) {
        this.showDts = val;
        if (this.index == 1 || val != 1) {
          this.forceUpdate("full");
          /*
            if (val)
            setScrollLock(this, true, true);
            else
            setScrollLock(this, false, false);
          */
          if (this.dtsAnnot)
            this.dtsAnnot.clear();

          if (this.dtsAnnotEss)
            this.dtsAnnotEss.clear();

          this.dtsAnnot = null;
          this.dtsAnnotEss = null;
        }
      }
    },
    setShowToolTip: function(val, ms) {
      val = val !== false;

      if (!val) {
        if (this.toolTipTimer) {
          //console.log("[setShowToopTip] cancel: tid="+this.toolTipTimer);
          clearTimeout(this.toolTipTimer);
        }
        if (!ms) ms = 1000;
        this.toolTipTimer = setTimeout(enableToolTip, ms, this);
      }

      if (val != this.showToolTip) {
        this.showToolTip = val;
        if (val) {
          this.forceUpdate("full");
          this.toolTipTimer = null;
        }
        //console.log("ToolTip "+(val ? "enabled!" : "disabled!"));
      }
    }
  };

  function ensureDiff(dv) {
    if (dv.diffOutOfDate) {
      dv.diff = getDiff(dv.orig.getValue(), dv.edit.getValue(), dv.mv.options.ignoreWhitespace);
      dv.chunks = getChunks(dv.diff);
      dv.diffOutOfDate = false;
      CodeMirror.signal(dv.edit, "updateDiff", dv.diff);
    }
  }

  var updating = false;

  function registerUpdate(dv) {
    var edit = {from: 0, to: 0, marked: []};
    var orig = {from: 0, to: 0, marked: []};

    var editDts = {from: 0, to: 0, marked: []};
    var origDts = {from: 0, to: 0, marked: []};
    var editDtsEss = {from: 0, to: 0, marked: [], tbl: {}};
    var origDtsEss = {from: 0, to: 0, marked: [], tbl: {}};

    var debounceChange, updatingFast = false;
    function update(mode) {
      updating = true;
      updatingFast = false;
      if (mode == "full") {
        if (dv.svg) clear(dv.svg);
        if (dv.mv.svg) clear(dv.mv.svg);
        if (dv.copyButtons) clear(dv.copyButtons);
        clearMarks(dv.edit, edit.marked, dv.classes);
        clearMarks(dv.orig, orig.marked, dv.classes);
        edit.from = edit.to = orig.from = orig.to = 0;
        clearMarksDts(dv.edit, editDts.marked);
        clearMarksDts(dv.orig, origDts.marked);
        editDts.from = editDts.to = origDts.from = origDts.to = 0;
      }
      ensureDiff(dv);
      if (dv.showDifferences) {
        updateMarks(dv.edit, dv.diff, edit, DIFF_INSERT, dv.classes);
        updateMarks(dv.orig, dv.diff, orig, DIFF_DELETE, dv.classes);
      }
      if ((dv.showDts & dv.index) > 0) {
        updateMarksDts(1, dv.edit, dv.diffDts, editDts);
        updateMarksDts(2, dv.orig, dv.diffDts, origDts);
      }

      if (dv.mv.options.connect == "align") alignChunks(dv);

      makeConnections(dv);

      if (dv.needsScrollSync != null) syncScroll(dv, dv.needsScrollSync)

      updating = false;
    }
    function setDealign(fast) {
      if (updating) return;
      dv.dealigned = true;
      set(fast);
    }
    function set(fast) {
      if (updating || updatingFast) return;
      clearTimeout(debounceChange);
      if (fast === true) updatingFast = true;
      debounceChange = setTimeout(update, fast === true ? 20 : 250);
    }
    function change(_cm, change) {
      if (!dv.diffOutOfDate) {
        dv.diffOutOfDate = true;
        edit.from = edit.to = orig.from = orig.to = 0;
      }
      // Update faster when a line was added/removed
      setDealign(change.text.length - 1 != change.to.line - change.from.line);
    }
    function swapDoc() {
      dv.diffOutOfDate = true;
      dv.dealigned = true;
      update("full");
    }
    dv.edit.on("change", change);
    dv.orig.on("change", change);
    dv.edit.on("swapDoc", swapDoc);
    dv.orig.on("swapDoc", swapDoc);
    if (dv.mv.options.connect == "align") {
      CodeMirror.on(dv.edit.state.trackAlignable, "realign", setDealign)
      CodeMirror.on(dv.orig.state.trackAlignable, "realign", setDealign)
    }
    dv.edit.on("viewportChange", function() { set(false); });
    dv.orig.on("viewportChange", function() { set(false); });

    dv.edit.on("keyHandled", function() { set(false); });
    dv.orig.on("keyHandled", function() { set(false); });
    dv.edit.onSearchClose = function() { set(false); };
    dv.orig.onSearchClose = function() { set(false); };

    update();
    return update;
  }

  function registerScroll(dv, otherDv) {
    dv.edit.on("scroll", function() {
      syncScroll(dv, true) && makeConnections(dv);
    });
    dv.orig.on("scroll", function() {
      syncScroll(dv, false) && makeConnections(dv);
      if (otherDv) syncScroll(otherDv, true) && makeConnections(otherDv);
    });
  }

  function syncScroll(dv, toOrig) {
    // Change handler will do a refresh after a timeout when diff is out of date
    if (dv.diffOutOfDate) {
      if (dv.lockScroll && dv.needsScrollSync == null) dv.needsScrollSync = toOrig
      return false;
    }
    dv.needsScrollSync = null
    if (!dv.lockScroll) return true;
    var editor, other, now = +new Date;
    if (toOrig) { editor = dv.edit; other = dv.orig; }
    else { editor = dv.orig; other = dv.edit; }
    // Don't take action if the position of this editor was recently set
    // (to prevent feedback loops)
    if (editor.state.scrollSetBy == dv && (editor.state.scrollSetAt || 0) + 250 > now) return false;

    var sInfo = editor.getScrollInfo();
    if (dv.mv.options.connect == "align") {
      targetPos = sInfo.top;
    } else {
      var halfScreen = .5 * sInfo.clientHeight, midY = sInfo.top + halfScreen;
      var mid = editor.lineAtHeight(midY, "local");

      var around;
      if (dv.showDifferences)
        around = chunkBoundariesAround(dv.chunks, mid, toOrig);
      else
        around = chunkBoundariesAroundDts(dv.diffDts, mid, toOrig);

      var off = getOffsets(editor, toOrig ? around.edit : around.orig);
      var offOther = getOffsets(other, toOrig ? around.orig : around.edit);
      var ratio = (midY - off.top) / (off.bot - off.top);
      var targetPos = (offOther.top - halfScreen) + ratio * (offOther.bot - offOther.top);

      var botDist, mix;
      // Some careful tweaking to make sure no space is left out of view
      // when scrolling to top or bottom.
      if (targetPos > sInfo.top && (mix = sInfo.top / halfScreen) < 1) {
        targetPos = targetPos * mix + sInfo.top * (1 - mix);
      } else if ((botDist = sInfo.height - sInfo.clientHeight - sInfo.top) < halfScreen) {
        var otherInfo = other.getScrollInfo();
        var botDistOther = otherInfo.height - otherInfo.clientHeight - targetPos;
        if (botDistOther > botDist && (mix = botDist / halfScreen) < 1)
          targetPos = targetPos * mix + (otherInfo.height - otherInfo.clientHeight - botDist) * (1 - mix);
      }
    }

    other.scrollTo(sInfo.left, targetPos);
    other.state.scrollSetAt = now;
    other.state.scrollSetBy = dv;
    return true;
  }

  function getOffsets(editor, around) {
    var bot = around.after;
    if (bot == null) bot = editor.lastLine() + 1;
    return {top: editor.heightAtLine(around.before || 0, "local"),
            bot: editor.heightAtLine(bot, "local")};
  }

  function setScrollLock(dv, val, action) {
    dv.lockScroll = val;
    if (val && action != false) syncScroll(dv, DIFF_INSERT) && makeConnections(dv);
    //dv.lockButton.innerHTML = val ? "\u21db\u21da" : "\u21db&nbsp;&nbsp;\u21da";
    (val ? CodeMirror.addClass : CodeMirror.rmClass)(dv.lockButton, "CodeMirror-merge-scrolllock-enabled");
  }

  // Updating the marks for editor content

  function removeClass(editor, line, classes) {
    var locs = classes.classLocation
    for (var i = 0; i < locs.length; i++) {
      editor.removeLineClass(line, locs[i], classes.chunk);
      editor.removeLineClass(line, locs[i], classes.start);
      editor.removeLineClass(line, locs[i], classes.end);
    }
  }

  function clearMarks(editor, arr, classes) {
    for (var i = 0; i < arr.length; ++i) {
      var mark = arr[i];
      if (mark instanceof CodeMirror.TextMarker)
        mark.clear();
      else if (mark.parent)
        removeClass(editor, mark, classes);
    }
    arr.length = 0;
  }

  // FIXME maybe add a margin around viewport to prevent too many updates
  function updateMarks(editor, diff, state, type, classes) {
    var vp = editor.getViewport();
    editor.operation(function() {
      if (state.from == state.to || vp.from - state.to > 20 || state.from - vp.to > 20) {
        clearMarks(editor, state.marked, classes);
        markChanges(editor, diff, type, state.marked, vp.from, vp.to, classes);
        state.from = vp.from; state.to = vp.to;
      } else {
        if (vp.from < state.from) {
          markChanges(editor, diff, type, state.marked, vp.from, state.from, classes);
          state.from = vp.from;
        }
        if (vp.to > state.to) {
          markChanges(editor, diff, type, state.marked, state.to, vp.to, classes);
          state.to = vp.to;
        }
      }
    });
  }

  function addClass(editor, lineNr, classes, main, start, end) {
    var locs = classes.classLocation, line = editor.getLineHandle(lineNr);
    for (var i = 0; i < locs.length; i++) {
      if (main) editor.addLineClass(line, locs[i], classes.chunk);
      if (start) editor.addLineClass(line, locs[i], classes.start);
      if (end) editor.addLineClass(line, locs[i], classes.end);
    }
    return line;
  }

  function markChanges(editor, diff, type, marks, from, to, classes) {
    var pos = Pos(0, 0);
    var top = Pos(from, 0), bot = editor.clipPos(Pos(to - 1));
    var cls = type == DIFF_DELETE ? classes.del : classes.insert;
    function markChunk(start, end) {
      var bfrom = Math.max(from, start), bto = Math.min(to, end);
      for (var i = bfrom; i < bto; ++i)
        marks.push(addClass(editor, i, classes, true, i == start, i == end - 1));
      // When the chunk is empty, make sure a horizontal line shows up
      if (start == end && bfrom == end && bto == end) {
        if (bfrom)
          marks.push(addClass(editor, bfrom - 1, classes, false, false, true));
        else
          marks.push(addClass(editor, bfrom, classes, false, true, false));
      }
    }

    var chunkStart = 0, pending = false;
    for (var i = 0; i < diff.length; ++i) {
      var part = diff[i], tp = part[0], str = part[1];
      if (tp == DIFF_EQUAL) {
        var cleanFrom = pos.line + (startOfLineClean(diff, i) ? 0 : 1);
        moveOver(pos, str);
        var cleanTo = pos.line + (endOfLineClean(diff, i) ? 1 : 0);
        if (cleanTo > cleanFrom) {
          if (pending) { markChunk(chunkStart, cleanFrom); pending = false }
          chunkStart = cleanTo;
        }
      } else {
        pending = true
        if (tp == type) {
          var end = moveOver(pos, str, true);
          var a = posMax(top, pos), b = posMin(bot, end);
          if (!posEq(a, b))
            marks.push(editor.markText(a, b, {className: cls}));
          pos = end;
        }
      }
    }
    if (pending) markChunk(chunkStart, pos.line + 1);
  }

  function clearMarksDts(editor, arr) {
    for (var i = 0; i < arr.length; ++i) {
      var mark = arr[i];
      if (mark instanceof CodeMirror.TextMarker) {
        mark.clear();
      }
    }
    arr.length = 0;
  }

  function updateMarksDts(idx, cm, diffDts, state) {
    var vp = cm.getViewport();
    //console.log('['+idx+'][updateMarksDts] vp=['+vp.from+'-'+vp.to+']', state);
    cm.operation(function() {

      if (state.from == state.to || vp.from - state.to > 20 || state.from - vp.to > 20) {
        clearMarksDts(cm, state.marked);
        markChangesDts(idx, cm, diffDts, state.marked, vp.from, vp.to);
        state.from = vp.from;
        state.to = vp.to;
      } else {
        if (vp.from < state.from) {
          markChangesDts(idx, cm, diffDts, state.marked, Math.max(vp.from-10, 0), state.from);
          state.from = vp.from;
        }
        if (vp.to > state.to) {
          markChangesDts(idx, cm, diffDts, state.marked, state.to, vp.to+10);
          state.to = vp.to;
        }
      }

    });
  }

  function findStartIndex(idx, edits, from, to) {
    if (from == 0) return 0;

    var i = 0;
    var j = edits.length;
    var e, v, v_, p;

    var count = 0;

    //console.log('['+idx+'][findStartIndex] i='+i+', j='+j+', vp=['+from+'-'+to+']');

    while (j - i > 2) {
      //console.log('i='+i+', j='+j);

      count += 1;
      if (count > 100)
        return 0;

      p = Math.floor((i + j) / 2);

      e = edits[p];

      if (e == undefined) return 0;

      v = e['line'+idx];
      v_ = e['line'+idx+'_'];

      //console.log('p='+p+', v='+v+', v_='+v_);

      if (v_ < from) {
        i = p;
      } else if (v > to) {
        j = p;
      } else {
        while (p > 0 && edits[p-1]['line'+idx+'_'] >= from) {
          count += 1;
          p -= 1;
        }
        i = p;
        break;
      }

      //console.log(' -> i='+i+', j='+j);
    }
    //console.log('['+idx+'][findStartIndex] i='+i+', v=['+edits[i]['line'+idx]+'-'+edits[i]['line'+idx+'_']+'], count='+count);

    return i;
  }

  function markChangesDts(idx, cm, diffDts, marks, from, to) {
    //console.log("["+idx+"][markChangesDts] marked="+marks.length+", vp=["+from+"-"+to+"]");

    var edits;

    if (idx == 1)
      edits = diffDts.edits1;
    else if (idx == 2)
      edits = diffDts.edits2;

    if (edits == undefined) return null;

    var doc = cm.getDoc();
    var top = new Pos(from, 0), bot = cm.clipPos(new Pos(to - 1, 0));

    var editDts, tag, lab, segs, cls, IGN, MISS;
    var seg, st, ed, st_pos, ed_pos, stl, edl, stl0, edl0;
    var i, j, k;
    var m, ms, segSkip, editBreak = false;

    // for edits
    var count = 0;
    var startIndex = findStartIndex(idx, edits, from, to);
    //console.log('['+idx+'][markChangesDts] startIndex='+startIndex+', edits.length='+edits.length);
    for (i = startIndex; i < edits.length; ++i) {
      count += 1;

      editDts = edits[i];

      if (editDts == null)
        continue;

      //console.log(i, editDts);

      tag = editDts["tag"];
      lab = "segments"+idx;

      //console.log('tag="'+tag+'",lab="'+lab+'"');

      segs = editDts[lab];

      if (segs == undefined)
        continue;

      cls = null;

      IGN = "IGNORED"+idx;
      MISS = "MISPARSED"+idx;

      switch (tag) {
      case "DELETE":
        cls = classesDts.del;
        break;
      case "INSERT":
        cls = classesDts.ins;
        break;
      case "RELABEL":
        cls = classesDts.rel;
        break;
      case "MOVREL":
        cls = classesDts.movrel;
        break;
      case "MOVE":
        cls = classesDts.mov;
        break;
      case IGN:
        cls = classesDts.ign;
        break;
      case MISS:
        cls = classesDts.miss;
        break;
      }

      if (cls == null)
        continue;

      stl0 = doc.posFromIndex(editDts['start'+idx]).line;
      edl0 = doc.posFromIndex(editDts['end'+idx]).line;

      for (j = 0; j < segs.length; j++) {
        seg = segs[j];
        st = seg["start"];
        ed = seg["end"] + 1;

        //console.log("["+cls+"] st="+st+", ed="+ed);

        st_pos = doc.posFromIndex(st);
        ed_pos = doc.posFromIndex(ed);
        stl = st_pos.line;
        edl = ed_pos.line;

        //console.log('['+idx+"][markChangesDts]["+cls+"]", stl, edl);

        //st_pos = posMax(top, st_pos);
        //ed_pos = posMin(bot, ed_pos);

        //console.log("["+cls+"]", st_pos, ed_pos);

        segSkip = false;

        ms = doc.findMarks(st_pos, ed_pos);
        for (k = 0; k < ms.length; k++) {
          if (ms[k].className == cls) {
            if (ms[k].editDts === editDts) {
              segSkip = true;
              break;
            }
          }
        }

        if (!segSkip) {
          m = doc.markText(st_pos, ed_pos, {className:cls,
                                            startStyle:cls+'-left',
                                            endStyle:cls+'-right',
                                            atomic:false});
          m.editDts = editDts;
          marks.push(m);

        }

        // if (editDts.tag == 'DELETE' || editDts.tag == 'INSERT')
        //   console.log("["+idx+"][markChangesDts] "+cls, st_pos, ed_pos);

        if (edl < from || to < stl) {
          if (stl0 > to) editBreak = true;
        }

      }

      if (editBreak) break;

    }
    //console.log("["+idx+"][markChangesDts] -> marked "+marks.length+', count='+count);

    return count;
  }

  // Updating the gap between editor and original

  function makeConnections(dv) {

    if ((dv.showDts & dv.index) > 0) {
      drawConnectorsDts(dv, dv.diffDts, false);
    } else {
      drawConnectorsDts(dv, dv.mv.diffDtsEss, true);
    }

    if (!dv.showDifferences) return;

    if (dv.svg) {
      clear(dv.svg);
      var w = dv.gap.offsetWidth;
      attrs(dv.svg, "width", w, "height", dv.gap.offsetHeight);
    }
    if (dv.copyButtons) clear(dv.copyButtons);

    var vpEdit = dv.edit.getViewport(), vpOrig = dv.orig.getViewport();
    var outerTop = dv.mv.wrap.getBoundingClientRect().top
    var sTopEdit = outerTop - dv.edit.getScrollerElement().getBoundingClientRect().top + dv.edit.getScrollInfo().top;
    var sTopOrig = outerTop - dv.orig.getScrollerElement().getBoundingClientRect().top + dv.orig.getScrollInfo().top;
    for (var i = 0; i < dv.chunks.length; i++) {
      var ch = dv.chunks[i];
      if (ch.editFrom <= vpEdit.to && ch.editTo >= vpEdit.from &&
          ch.origFrom <= vpOrig.to && ch.origTo >= vpOrig.from)
        drawConnectorsForChunk(dv, ch, sTopOrig, sTopEdit, w);
    }
  }

  function getMatchingOrigLine(editLine, chunks) {
    var editStart = 0, origStart = 0;
    for (var i = 0; i < chunks.length; i++) {
      var chunk = chunks[i];
      if (chunk.editTo > editLine && chunk.editFrom <= editLine) return null;
      if (chunk.editFrom > editLine) break;
      editStart = chunk.editTo;
      origStart = chunk.origTo;
    }
    return origStart + (editLine - editStart);
  }

  // Combines information about chunks and widgets/markers to return
  // an array of lines, in a single editor, that probably need to be
  // aligned with their counterparts in the editor next to it.
  function alignableFor(cm, chunks, isOrig) {
    var tracker = cm.state.trackAlignable
    var start = cm.firstLine(), trackI = 0
    var result = []
    for (var i = 0;; i++) {
      var chunk = chunks[i]
      var chunkStart = !chunk ? 1e9 : isOrig ? chunk.origFrom : chunk.editFrom
      for (; trackI < tracker.alignable.length; trackI += 2) {
        var n = tracker.alignable[trackI] + 1
        if (n <= start) continue
        if (n <= chunkStart) result.push(n)
        else break
      }
      if (!chunk) break
      result.push(start = isOrig ? chunk.origTo : chunk.editTo)
    }
    return result
  }

  // Given information about alignable lines in two editors, fill in
  // the result (an array of three-element arrays) to reflect the
  // lines that need to be aligned with each other.
  function mergeAlignable(result, origAlignable, chunks, setIndex) {
    var rI = 0, origI = 0, chunkI = 0, diff = 0
    outer: for (;; rI++) {
      var nextR = result[rI], nextO = origAlignable[origI]
      if (!nextR && nextO == null) break

      var rLine = nextR ? nextR[0] : 1e9, oLine = nextO == null ? 1e9 : nextO
      while (chunkI < chunks.length) {
        var chunk = chunks[chunkI]
        if (chunk.origFrom <= oLine && chunk.origTo > oLine) {
          origI++
          rI--
          continue outer;
        }
        if (chunk.editTo > rLine) {
          if (chunk.editFrom <= rLine) continue outer;
          break
        }
        diff += (chunk.origTo - chunk.origFrom) - (chunk.editTo - chunk.editFrom)
        chunkI++
      }
      if (rLine == oLine - diff) {
        nextR[setIndex] = oLine
        origI++
      } else if (rLine < oLine - diff) {
        nextR[setIndex] = rLine + diff
      } else {
        var record = [oLine - diff, null, null]
        record[setIndex] = oLine
        result.splice(rI, 0, record)
        origI++
      }
    }
  }

  function findAlignedLines(dv, other) {
    var alignable = alignableFor(dv.edit, dv.chunks, false), result = []
    if (other) for (var i = 0, j = 0; i < other.chunks.length; i++) {
      var n = other.chunks[i].editTo
      while (j < alignable.length && alignable[j] < n) j++
      if (j == alignable.length || alignable[j] != n) alignable.splice(j++, 0, n)
    }
    for (var i = 0; i < alignable.length; i++)
      result.push([alignable[i], null, null])

    mergeAlignable(result, alignableFor(dv.orig, dv.chunks, true), dv.chunks, 1)
    if (other)
      mergeAlignable(result, alignableFor(other.orig, other.chunks, true), other.chunks, 2)

    return result
  }

  function alignChunks(dv, force) {
    if (!dv.dealigned && !force) return;
    if (!dv.orig.curOp) return dv.orig.operation(function() {
      alignChunks(dv, force);
    });

    dv.dealigned = false;
    var other = dv.mv.left == dv ? dv.mv.right : dv.mv.left;
    if (other) {
      ensureDiff(other);
      other.dealigned = false;
    }
    var linesToAlign = findAlignedLines(dv, other);

    // Clear old aligners
    var aligners = dv.mv.aligners;
    for (var i = 0; i < aligners.length; i++)
      aligners[i].clear();
    aligners.length = 0;

    var cm = [dv.edit, dv.orig], scroll = [];
    if (other) cm.push(other.orig);
    for (var i = 0; i < cm.length; i++)
      scroll.push(cm[i].getScrollInfo().top);

    for (var ln = 0; ln < linesToAlign.length; ln++)
      alignLines(cm, linesToAlign[ln], aligners);

    for (var i = 0; i < cm.length; i++)
      cm[i].scrollTo(null, scroll[i]);
  }

  function alignLines(cm, lines, aligners) {
    var maxOffset = 0, offset = [];
    for (var i = 0; i < cm.length; i++) if (lines[i] != null) {
      var off = cm[i].heightAtLine(lines[i], "local");
      offset[i] = off;
      maxOffset = Math.max(maxOffset, off);
    }
    for (var i = 0; i < cm.length; i++) if (lines[i] != null) {
      var diff = maxOffset - offset[i];
      if (diff > 1)
        aligners.push(padAbove(cm[i], lines[i], diff));
    }
  }

  function padAbove(cm, line, size) {
    var above = true;
    if (line > cm.lastLine()) {
      line--;
      above = false;
    }
    var elt = document.createElement("div");
    elt.className = "CodeMirror-merge-spacer";
    elt.style.height = size + "px"; elt.style.minWidth = "1px";
    return cm.addLineWidget(line, elt, {height: size, above: above, mergeSpacer: true, handleMouseEvents: true});
  }

  function getRectPathDts(cm, bound, pos0, pos1) {
    var dx = bound.left;
    var dy = bound.top;

    //console.log("dx="+dx+",dy="+dy);

    var doc = cm.getDoc();

    var p0 = cm.charCoords(pos0, "page");
    var p1 = cm.charCoords(pos1, "page");

    //console.log("p0={left:"+p0.left+",right:"+p0.right+",top:"+p0.top+",bottom:"+p0.bottom+"}");
    //console.log("p1={left:"+p1.left+",right:"+p1.right+",top:"+p1.top+",bottom:"+p1.bottom+"}");

    var px = p0.left - dx, py = p0.top - dy;
    var bx = px;
    /*
      if (pos0.line != pos1.line)
      console.log("!!! multiline");
    */
    var lastLines;

    var start = px+" "+py;

    if (pos0.ch == 0 || pos0.line == pos1.line) {
      lastLines = "L "+start+" ";
    } else {
      var pl = cm.charCoords(new Pos(pos0.line, 0), "page");
      bx = pl.left - dx;
      lastLines = "L "+bx+" "+(pl.bottom-dy)+" L "+px+" "+(p0.bottom-dy)+" L "+start+" ";
    }

    var path = "M "+start+" ";

    for (var ln = pos0.line; ln < pos1.line + 1; ln++) {
      var line = doc.getLine(ln);

      if (ln == pos1.line) { // last line
        var px0 = p1.right - dx;
        var py0 = p1.bottom - dy;
        path += "L "+px0+" "+(p1.top-dy)+" L "+px0+" "+py0+" ";
        path += "L "+bx+" "+py0+" "+lastLines;

      } else {
        var p = cm.charCoords(new Pos(ln, line.length - 1), "page");
        var px0 = p.right - dx;
        var py0 = p.bottom - dy;
        path += "L "+px0+" "+(p.top-dy)+" L "+px0+" "+py0+" ";
      }

    }

    //console.log('path="'+path+'"');

    return path;
  }

  function createClip(baseBound, defs, id, cm, clip, rect) {
    if (clip == undefined)
      clip = defs.appendChild(document.createElementNS(svgNS, "clipPath"));
    attrs(clip, "id", id/*, "clipPathUnits", "objectBoundingBox"*/);
    var elem = cm.getWrapperElement();
    var bound = elem.getBoundingClientRect();

    //console.log("[createClip] id:"+id+", bound: left="+bound.left+", top="+bound.top+", height="+bound.height+", width="+bound.width);

    // to avoid dialog
    var dialogHeight = 0;
    var dialogs = elem.getElementsByClassName("CodeMirror-dialog");
    for (var i = 0; i < dialogs.length; i++) {
      var r = dialogs[i].getBoundingClientRect();
      if (r) {
        dialogHeight = r.height;
        break;
      }
    }

    // to avoid gutter
    var lineGutterWidth = 0;
    var x = bound.left;
    var width = bound.width;
    var lineGutter = cm.display.gutters;
    if (lineGutter) {
      var b = lineGutter.getBoundingClientRect();
      //console.log("[createClip] b: right="+b.right+", top="+b.top+", height="+b.height+", width="+b.width);
      lineGutterWidth = b.width;
      x = b.right + 1;
      width -= lineGutterWidth;
    }

    // to avoid vertical simplescrollbar
    var vscrollbarWidth = 0;
    var vscrollbars = elem.getElementsByClassName("CodeMirror-simplescroll-vertical");
    for (var i = 0; i < vscrollbars.length; i++) {
      var r = vscrollbars[i].getBoundingClientRect();
      if (r) {
        vscrollbarWidth = r.width;
        break;
      }
    }
    //console.log("vscrollbarWidth="+vscrollbarWidth);
    width -= vscrollbarWidth;

    // to avoid horizontal simplescrollbar
    var hscrollbarHeight = 0;
    var hscrollbars = elem.getElementsByClassName("CodeMirror-simplescroll-horizontal");
    for (var i = 0; i < hscrollbars.length; i++) {
      var r = hscrollbars[i].getBoundingClientRect();
      if (r) {
        hscrollbarHeight = r.height;
        break;
      }
    }
    //console.log("hscrollbarHeight="+hscrollbarHeight);


    //console.log("[createClip] lineGutterWidth="+lineGutterWidth);

    var r = {x:x-baseBound.left,y:bound.top-baseBound.top+dialogHeight,
             height:bound.height-dialogHeight-hscrollbarHeight,width:width};

    if (rect == undefined)
      rect = clip.appendChild(document.createElementNS(svgNS, "rect"));
    attrs(rect, "x", r.x, "y", r.y, "height", r.height, "width", r.width);

    return [x, r, dialogHeight > 0, lineGutterWidth];
  }

  function mergeClip(shown0, shown1, rect0, rect1, lgwidth, defs, id) {
    var clip = document.createElementNS(svgNS, "clipPath");
    attrs(defs.appendChild(clip), "id", id);
    var p =
      "M "+rect0.x+" "+rect0.y+" v "+rect0.height+
      " L "+(rect1.x+rect1.width)+" "+(rect1.y+rect1.height)+
      " v "+(-rect1.height);

    if (shown0 && !shown1)
      p += " L "+(rect0.x+rect0.width)+" "+(rect1.y)+" v "+(rect1.height-rect0.height);
    else if (!shown0 && shown1)
      p += " h "+(-rect1.width-lgwidth-1)+" v "+(rect1.height-rect0.height);

    p += " z";

    //console.log("[mergeClip] p="+p);

    attrs(clip.appendChild(document.createElementNS(svgNS, "path")),
          "d", p);
  }

  //var MAX_ANNOTATIONS = 1000;

  function DtsScrollbarAnnotation(cmL, cmR, diffDts) {

    if (!diffDts)
      return;

    if (cmL.annotateScrollbar && cmR.annotateScrollbar) {

      this.annotationL = cmL.annotateScrollbar("DTS-scrollbar-marks");
      this.annotationR = cmR.annotateScrollbar("DTS-scrollbar-marks");

      this.marksL = [];
      this.marksR = [];

      var docL = cmL.getDoc();
      var docR = cmR.getDoc();

      for (var i = 0; i < diffDts.length; ++i) {

        // if (i > MAX_ANNOTATIONS)
        //   break;

        var edit = diffDts[i];

        if (edit == null)
          continue;

        var tag = edit["tag"];

        switch (tag) {
        case "DELETE":
          //console.log("[DtsScrollbarAnnotation]["+i+"] tag="+tag);
          var m = {from:docL.posFromIndex(edit.start1),to:docL.posFromIndex(edit.end1+1)};
          this.marksL.push(m);
          break;

        case "INSERT":
          //console.log("[DtsScrollbarAnnotation]["+i+"] tag="+tag);
          var m = {from:docR.posFromIndex(edit.start2),to:docR.posFromIndex(edit.end2+1)};
          this.marksR.push(m);
          break;

        case "RELABEL":
        case "MOVREL":
        case "MOVE":
        //case "CORR":
          //console.log("[DtsScrollbarAnnotation]["+i+"] tag="+tag);
          var mL = {from:docL.posFromIndex(edit.start1),to:docL.posFromIndex(edit.end1+1)};
          var mR = {from:docR.posFromIndex(edit.start2),to:docR.posFromIndex(edit.end2+1)};
          this.marksL.push(mL);
          this.marksR.push(mR);
          break;

        default:
          break;
        }

      }
      var cmp = function(a, b) {
        var x = a.from.line - b.from.line;
        if (x === 0)
          x = b.to.line - a.to.line;
        return x;
      }
      this.marksL.sort(cmp);
      this.marksR.sort(cmp);
    }
  }

  DtsScrollbarAnnotation.prototype.mark = function() {
    if (this.annotationL && this.annotationR) {
      //console.log("marking...");
      if (this.marksL.length > 0) {
        var from = this.marksL[0].from.line;
        var to = this.marksL[0].to.line;
        var marks = [this.marksL[0]];
        for (var i = 0; i < this.marksL.length; i++) {
          var m = this.marksL[i];
          //console.log('L: ['+m.from.line+'-'+m.to.line+']');
          if (from !== undefined && to !== undefined) {
            if (to < m.from.line) {
              from = m.from.line;
              to = m.to.line;
              marks.push(m);
            }
          }
        }
        this.marksL = marks;
      }
      if (this.marksR.length > 0) {
        var from = this.marksR[0].from.line;
        var to = this.marksR[0].to.line;
        var marks = [this.marksR[0]];
        for (var i = 0; i < this.marksR.length; i++) {
          var m = this.marksR[i];
          //console.log('R: ['+m.from.line+'-'+m.to.line+']');
          if (from !== undefined && to !== undefined) {
            if (to < m.from.line) {
              from = m.from.line;
              to = m.to.line;
              marks.push(m);
            }
          }
        }
        this.marksR = marks;
      }
      try { // the following occasionally fails
        this.annotationL.update(this.marksL);
        this.annotationR.update(this.marksR);
      } catch (exn) {
        console.log("caught exception: "+exn);
      }
    }
  };

  DtsScrollbarAnnotation.prototype.clear = function() {
    //try { // the following occasionally fails

    try {
      if (this.annotationL !== null)
        this.annotationL.clear();
    } catch (exn) {
      console.log("annotationL: caught exception: "+exn);
    }
    try {
      if (this.annotationR !== null)
        this.annotationR.clear();
    } catch (exn) {
      console.log("annotationR: caught exception: "+exn);
    }

    //} catch (exn) {
    //  console.log("caught exception: "+exn);
    //}
  };

  function convOfs(ofs) {
    var st = ofs[0];
    var ed = ofs[1];
    var v;
    if (typeof ed == "string") {
      v = ed;
      ed = st;
    } else {
      v = ofs[2];
    }
    return [st, ed, v];
  }

  function essToDiffDts(ess0, ess1, ess01) {
    var diffDts = [];

    for (var i = 0; i < ess0.length; i++) {
      var ofs = convOfs(ess0[i]);
      diffDts.push({tag:"DELETE",start1:ofs[0],end1:ofs[1],v1:ofs[2]});
    }
    for (var i = 0; i < ess1.length; i++) {
      var ofs = convOfs(ess1[i]);
      diffDts.push({tag:"INSERT",start2:ofs[0],end2:ofs[1],v2:ofs[2]});
    }
    for (var i = 0; i < ess01.length; i++) {
      var ofs0 = convOfs(ess01[i][0]);
      var ofs1 = convOfs(ess01[i][1]);
      diffDts.push({tag:"CORR",
                    start1:ofs0[0],end1:ofs0[1],start2:ofs1[0],end2:ofs1[1],
                    v1:ofs0[2],v2:ofs1[2]});
    }

    return diffDts;
  }

  function disableToolTip(dv, ms) {
    dv.setShowToolTip(false, ms);
  }

  function enableToolTip(dv) {
    dv.setShowToolTip(true);
  }

  function ToolTip(svg, frame, dv, v) {
    this.svg = svg;
    this.frame = frame;
    this.dv = dv
    var bound = this.bound = dv.mv.glasspane.getBoundingClientRect();
    this.v = v;
    this.textElem = document.createElementNS(svgNS, "text");
    this.textElem.textContent = this.v;
    this.rectElem = document.createElementNS(svgNS, "rect");

    this.show = function() {
      var fb = this.frame.getBoundingClientRect();
      var x = fb.left + fb.width / 2;

      var y0 = bound.top < fb.top ? fb.top - bound.top : 0;
      var y1 = fb.bottom < bound.bottom ? fb.bottom - bound.top : bound.height;
      var y = (y0 + y1) / 2;

      //console.log("x="+x+",y="+y);

      this.svg.appendChild(this.rectElem);
      this.svg.appendChild(this.textElem);

      attrs(this.textElem, "x", x, "y", y);

      var tbb = this.textElem.getBBox();

      //console.log("tbb: height="+tbb.height+",width="+tbb.width);

      var margin = 10;

      attrs(this.rectElem,
            "x", x-margin, "y", y-tbb.height-margin,
            "height", tbb.height+2*margin, "width", tbb.width+2*margin,
            "class", "DTS-tooltip");

    };
    this.hide = function() {
      try {
        this.svg.removeChild(this.textElem);
        this.svg.removeChild(this.rectElem);
      } catch (exn) {
      }
    }
  }

  function showProperties(obj) {
    var o = obj;
    while (o) {
      var ks = Object.keys(o);
      for (var i = 0; i < ks.length; i++) {
        var k = ks[i];
        var v = obj[k];
          if (typeof v != "function")
            console.log("  "+k+"="+v);
      }
      o = Object.getPrototypeOf(o);
    }
  }

  function copyMouseEvent(ev) {
    var evCopy = document.createEvent("MouseEvents");
    evCopy.initMouseEvent(ev.type, ev.bubbles, ev.cancelable, ev.view, ev.detail,
                          ev.screenX, ev.screenY, ev.clientX, ev.clientY,
                          ev.ctrlKey, ev.altKey, ev.shiftKey, ev.metaKey,
                          ev.button, ev.relatedTarget);
    return evCopy;
  }

  function copyMouseScrollEvent(ev) {
    var evCopy = document.createEvent("MouseScrollEvents");
    evCopy.initMouseEvent(ev.type, ev.bubbles, ev.cancelable, ev.view, ev.detail,
                          ev.screenX, ev.screenY, ev.clientX, ev.clientY,
                          ev.ctrlKey, ev.altKey, ev.shiftKey, ev.metaKey,
                          ev.button, ev.relatedTarget, ev.axis);
    return evCopy;
  }

  function makeEssEnterHandler(tt) {
    return function(ev) {
      //console.log("enter: type="+ev.type+" v="+tt.v);
      tt.show();
    }
  }
  function makeEssExitHandler(tt) {
    return function(ev) {
      //console.log("exit: type="+ev.type+" v="+tt.v);
      tt.hide();
    }
  }

  function makeWheelEventForwarder(dv, cm) { // for chrome
    return function(ev) {
      if (ev instanceof WheelEvent) {
        var evCopy =
          new WheelEvent(ev.type, {deltaX:ev.deltaX,deltaY:ev.deltaY,deltaZ:ev.deltaZ,
                                   deltaMode:ev.deltaMode});
        //console.log("forward: type="+ev.type+",deltaY="+ev.deltaY);
        cm.getScrollerElement().dispatchEvent(evCopy);
      }
    }
  }
  function makeMouseScrollEventForwarder(tt) { // for gecko
    var dv = tt.dv;
    var frame = tt.frame;
    return function(ev) {
      if (ev instanceof MouseScrollEvent) {
        disableToolTip(dv, 1000);
        var evCopy = copyMouseScrollEvent(ev);
/*
        console.log("forward: type="+ev.type+", detail="+ev.detail
                    +", page="+ev.pageX+","+ev.pageY
                    +", screen="+ev.screenX+","+ev.screenY
                    +", client="+ev.clientX+","+ev.clientY
                   );
*/
        attrs(frame, "display", "none");
        var elem = document.elementFromPoint(ev.pageX, ev.pageY);
        attrs(frame, "display", "inline");

        //console.log("forward: nodeName="+elem.nodeName+" textContent="+elem.textContent);
        //elem.dispatchEvent(evCopy);
      }
    }
  }
  function makeMouseEventForwarder(cm) {
    return function(ev) {
      if (ev instanceof MouseEvent) {
        var evCopy = copyMouseEvent(ev);
        //console.log("forward: type="+ev.type+", detail="+ev.detail);
        cm.getScrollerElement().dispatchEvent(evCopy);
      }
    }
  }

  function handleEditClick(cm, ev) {
    var pos = cm.coordsChar({left:ev.pageX,top:ev.pageY}, "page");
    var ms = cm.getDoc().findMarksAt(pos);
    var ed;
    //var range, from = null, to = null;
    var modified = false;

    for (var i = 0; i < ms.length; i++) {
/*
      range = ms[i].find();
      if (from == null)
        from = range.from;
      else
        from = range.from > from ? range.from : from;
      if (to == null)
        to = range.to;
      else
        to = range.to < to ? range.to : to;
*/
      ed = ms[i].editDts;

      //console.log('['+i+'] '+ed.tag+' (visible='+ed.connectorVisible+')');

      switch (ed.tag) {
      case "RELABEL":
      case "MOVREL":
      case "MOVE":
        ed.connectorVisible = !ed.connectorVisible;
        modified = true;
        break;
      }
    }
/*
    if (from != null && to != null) {
      var text = cm.getDoc().getRange(from, to);
      console.log(from, to, text);
      var elm = elt('span', text, 'DTS-selected');
      from.line -=1;
      cm.addWidget(from, elm, false);
    }
*/
    if (modified) {
      clear(cm.dv.mv.svg);
      if (cm.dv.mv.hasLeft) makeConnections(cm.dv.mv.left);
      if (cm.dv.mv.hasRight) makeConnections(cm.dv.mv.right);
    }

  }

  function drawConnectorsDts(dv, diffDts, isEss) {

    //console.log("entering drawConnectorsDts: "+dv.type);

    if (!dv.mv || !diffDts)
        return;

    var bound = dv.mv.bound;

    if (!bound)
        return;

    var isLeft = dv.type == "left";

    var svg = dv.mv.svg;

    if (svg) {
      //clear(svg);
      attrs(svg,
            //"shape-rendering", "optimizeSpeed",
            "width", dv.mv.glasspane.offsetWidth,
            "height", dv.mv.glasspane.offsetHeight);
    }

    var sTopEdit = dv.edit.getScrollInfo().top, sTopOrig = dv.orig.getScrollInfo().top;

    var origCM = dv.orig, origDoc = origCM.getDoc();
    var editCM = dv.edit, editDoc = editCM.getDoc();

    var origClipId = dv.type+"Clip";
    var editClipId = "editClip";

    var dfs = dv.mv.defs;
    /*var dfs = svg.appendChild(dv.mv.defs);
    if (dfs)
      clear(dfs);*/

    var origXRectD = createClip(bound, dfs, origClipId, origCM,
                                isLeft ? dv.mv.clipL : dv.mv.clipR,
                                isLeft ? dv.mv.rectL : dv.mv.rectR);
    var editXRectD = createClip(bound, dfs, editClipId, editCM, dv.mv.clip, dv.mv.rect);

    //
    var origDshown = origXRectD[2];
    var editDshown = editXRectD[2];

    if (origDshown || editDshown) {
      mergeClip(origDshown, editDshown, origXRectD[1], editXRectD[1], editXRectD[3], dfs, "glasspaneClip");

    } else {
      var leftCM = isLeft ? origCM : editCM;
      //var gclip = dv.mv.gclip;
      //attrs(dfs.appendChild(gclip), "id", "glasspaneClip");
      attrs(dv.mv.gclip, "id", "glasspaneClip");
      var gbound = dv.mv.glasspane.getBoundingClientRect();

      //console.log("["+dv.type+"] gbound: left="+gbound.left+", top="+gbound.top+", height="+gbound.height+", width="+gbound.width);

      var x = 0;
      var width = gbound.width;
      var gutter = leftCM.display.gutters;
      if (gutter) {
        var gb = gutter.getBoundingClientRect();
        x += gb.width+1;
        width -= gb.width+1;
      }
      //attrs(gclip.appendChild(dv.mv.grect),
      attrs(dv.mv.grect,
            "x", x, "y", 1, "height", gbound.height-2, "width", width);
    }

    //

    var origX = origXRectD[0];
    var editX = editXRectD[0];

    var cms   = new Array(editCM, origCM);
    var docs  = new Array(editDoc, origDoc);
    var clips = new Array(editClipId, origClipId);

    var sposs = new Array(2);
    var eposs = new Array(2);

    //console.log("num of edits: "+diffDts.length);

    var curveMov    = "";
    var curveRel    = "";
    var curveMovRel = "";
    var curveAlign  = "";
    var curveCorr   = "";
    //var rectPaths = new Array("", "");
    var rectPathsA = new Array([], []);

    for (var idx = 0; idx < diffDts.length; idx++) {

      var editDts = diffDts[idx];

      if (editDts == null) continue;

      var tag = editDts["tag"];

      if (tag != 'ALIGN' && !editDts.connectorVisible) continue;

      //console.log("tag="+tag);

      var skips = new Array(false, false);
      var paths = new Array("", "");
      var vps   = new Array(dv.edit.getViewport(), dv.orig.getViewport());

      var rlen = 0;

      for (var i = 0; i < 2; i++) { // check visibility
        var i1 = i + 1;
        var slab = "start"+i1;
        var elab = "end"+i1;
        var llab = "line"+i1;

        if (slab in editDts && elab in editDts) {
          var st = editDts[slab], ed = editDts[elab];

          if (st <= ed) {
            rlen = ed - st;

            //console.log("editDts["+slab+"]="+st+",editDts["+elab+"]="+ed);

            var spos = docs[i].posFromIndex(st);
            var sch = docs[i].getLine(spos.line).charAt(spos.ch);
            if (sch == "")
              spos = new Pos(spos.line + 1, 0);

            sposs[i] = spos;

            var epos = eposs[i] = docs[i].posFromIndex(ed);

            //console.log("spos.line="+spos.line+", epos.line="+epos.line);
            //console.log("vps["+i+"].from="+vps[i].from+", vps["+i+"].to="+vps[i].to);

            skips[i] = spos.line > vps[i].to || epos.line < vps[i].from

            /*if (!skips[i]) {
              paths[i] += getRectPathDts(cms[i], bound, spos, epos); // for frames
            }*/
          }

        } else if (llab in editDts) { // for ALIGN
          var ln = editDts[llab];

          //console.log("["+i+"] editDts["+llab+"]="+ln);

          var cmln = ln - 1; // ???

          var line = docs[i].getLine(cmln);
          //console.log(docs[i]);
          //console.log("["+i+"] "+ln+":'"+line+"'");

          if (line) {
            for (var ch = 0; ch < line.length; ch++) {
              if (line.charAt(ch).match(/\S/g)) {
                //console.log("["+i+"] ch="+ch+" char="+line.charAt(ch));
                break;
              }
            }

            var spos = sposs[i] = new Pos(cmln, ch);

            //console.log("sposs["+i+"]=("+spos.line+","+spos.ch+")");

            var epos = eposs[i] = new Pos(cmln, line.length - 1);

            //console.log("vps["+i+"].from="+vps[i].from+", vps["+i+"].to="+vps[i].to);

            skips[i] = spos.line > vps[i].to || epos.line < vps[i].from

            //console.log("["+dv.type+"]["+i+"] skip="+skips[i]);
          } else {
            //console.log(docs[i]);
            //console.log("["+i+"] "+ln+":'"+line+"'");
            skips[i] = true;
          }
        }

      } // check visibility


      if (skips[0] && skips[1])
        continue;

/*
      for (var i = 0; i < 2; i++) { // for segments
        var lab = "segments"+(i+1);

        if (lab in editDts) {
          //console.log("lab="+lab);

          var doc = docs[i];
          var segs = editDts[lab];

          for (var j = 0; j < segs.length; j++) {
            var st = segs[j]["start"], ed = segs[j]["end"];

            if (st > ed) continue;

            //console.log("segs["+j+"][start]="+st+",segs["+j+"][end]="+ed);

            var spos = doc.posFromIndex(st);
            var sch = doc.getLine(spos.line).charAt(spos.ch);
            if (sch == "")
              spos = new Pos(spos.line + 1, 0);

            var epos = doc.posFromIndex(ed);

            //console.log("["+dv.type+"] spos={line:"+spos.line+",ch:"+spos.ch+"},epos={line:"+epos.line+",ch:"+epos.ch+"}");

            if (spos.line > vps[i].to || epos.line < vps[i].from)
              continue;

            paths[i] += getRectPathDts(cms[i], bound, spos, epos); // for frames
          }

        }

      } // for segments
*/

      if (isEss) {

        for (var i = 0; i < 2; i++) {
          if (paths[i] != "") {
            var vlab = "v"+(i+1);
            var v = editDts[vlab] || "?";
            paths[i] += "z";
            rectPathsA[i].push([paths[i], v, rlen]);
          }
        }

      }/* else {

        for (var i = 0; i < 2; i++) {
          //console.log("paths["+i+"]="+paths[i])
          if (paths[i] != "") {
            paths[i] += "z";
            //console.log('paths['+i+']="'+paths[i]+'"');
            rectPaths[i] += paths[i]+" ";
          }
        }

      }*/

      // Curves

      var curve = null;

      switch (tag) {
      case "DELETE":
        break;
      case "INSERT":
        break;

      case "RELABEL":
      case "MOVREL":
      case "MOVE":
      case "ALIGN":
      case "CORR": // for essentials

        //console.log("drawing connector for "+tag);

        var spos1 = sposs[0], epos1 = eposs[0], spos2 = sposs[1], epos2 = eposs[1];
        var dx = bound.left, dy = bound.top;

        var fromP, toP;
        var fromX, fromY, toX, toY, mx;

        if (isLeft) {
          fromP = editCM.charCoords(spos1, "page");

          if (spos2.line == epos2.line)
            toP = origCM.charCoords(epos2, "page");
          else
            toP = origCM.charCoords(new Pos(spos2.line,
                                            origDoc.getLine(spos2.line).length - 1), "page");

          fromX = fromP.left - dx;
          toX = Math.max(toP.right, origX) - dx;

          //console.log("["+dv.type+"] fromX="+fromX+", toX="+toX);

        } else {
          if (spos1.line == epos1.line)
            fromP = editCM.charCoords(epos1, "page");
          else
            fromP = editCM.charCoords(new Pos(spos1.line,
                                              editDoc.getLine(spos1.line).length - 1), "page");

          toP = origCM.charCoords(spos2, "page");

          fromX = Math.max(fromP.right, editX) - dx;
          toX = toP.left - dx;

          //console.log("["+dv.type+"] fromX="+fromX+", toX="+toX);
        }
        fromY = fromP.top - dy;
        toY = toP.top - dy;

        mx = Math.abs(toX + fromX) / 2;

        //console.log("["+dv.type+"] fromX="+fromX+", fromY="+fromY+", toX="+toX+", toY="+toY+", mx="+mx);

        fromX = Math.floor(fromX);
        fromY = Math.floor(fromY);
        toX = Math.floor(toX);
        toY = Math.floor(toY);
        mx = Math.floor(mx);

        curve = "M "+fromX+" "+fromY+" C "+mx+" "+fromY+" "+mx+" "+toY+" "+toX+" "+toY;

        break;

      default:
        break;
      }

      if (curve != null) {

        //console.log('curve="'+curve+'"');

        switch (tag) {
        case "MOVE":
          curveMov += curve+" ";
          break;

        case "RELABEL":
          curveRel += curve+" ";
          break;

        case "MOVREL":
          curveMovRel += curve+" ";
          break;

        case "ALIGN":
          curveAlign += curve+" ";
          break;

        case "CORR":
          curveCorr += curve+" ";
          break;

        default:
          break;
        }
      }

    } // for(var i = 0; i < diffDts.length; i++)


    if (isEss) {

      var cmp = function (a0, a1) {
        var l0 = a0[2];
        var l1 = a1[2];
        if (l0 < l1)
          return 1;
        else if (l0 > l1)
          return -1;
        return 0;
      };

      for (var i = 0; i < 2; i++) {

        var paths = rectPathsA[i];

        var npaths = paths.length;

        if (npaths > 0) {
          paths = paths.sort(cmp);

          for (var j = 0; j < npaths; j++) {
            var frame = document.createElementNS(svgNS, "path");
            svg.appendChild(frame);

            var v = paths[j][1];

            //console.log("["+i+"]["+j+"]["+v+"]");

            if (dv.showToolTip) {
              var tt = new ToolTip(svg, frame, dv, v);

              CodeMirror.on(frame, "mouseover", makeEssEnterHandler(tt));
              CodeMirror.on(frame, "mouseout", makeEssExitHandler(tt));
              CodeMirror.on(frame, "wheel", makeWheelEventForwarder(dv, cms[i]));
              frame.addEventListener("DOMMouseScroll", makeMouseScrollEventForwarder(tt));
            }

            var path = paths[j][0];

            //console.log("["+i+"]["+j+"]["+v+"] path="+path);

            attrs(frame,
                  "d", path,
                  "clip-path", "url(#"+clips[i]+")",
                  "pointer-events", "visibleFill",
                  "class", classesDts.frameEss);
          }
        }
      }

    }/* else {
      for (var i = 0; i < 2; i++) {
        //console.log('rectPaths['+i+']="'+rectPaths[i]+'"');
        if (rectPaths[i] != "") {
          attrs(svg.appendChild(dv.mv.rects[i]),
                "d", rectPaths[i],
                "clip-path", "url(#"+clips[i]+")",
                "class", classesDts.frame);
        }
      }
    }*/

    //console.log("drawing connectors:", dv.type);

    if (curveMov != "") {
      //console.log('curveMov="'+curveMov+'"');
      attrs(svg.appendChild(isLeft ? dv.mv.connectorMovL : dv.mv.connectorMov),
            "d", curveMov,
            "clip-path", "url(#glasspaneClip)",
            "class", classesDts.cmov);
    }

    if (curveRel != "") {
      //console.log('curveRel="'+curveRel+'"');
      attrs(svg.appendChild(isLeft ? dv.mv.connectorRelL : dv.mv.connectorRel),
            "d", curveRel,
            "clip-path", "url(#glasspaneClip)",
            "class", classesDts.crel);
    }

    if (curveMovRel != "") {
      //console.log('curveMovRel="'+curveMovRel+'"');
      attrs(svg.appendChild(isLeft ? dv.mv.connectorMovRelL : dv.mv.connectorMovRel),
            "d", curveMovRel,
            "clip-path", "url(#glasspaneClip)",
            "class", classesDts.cmovrel);
    }

    if (curveAlign != "") {
      //console.log('['+dv.type+'] curveAlign="'+curveAlign+'"');
      attrs(svg.appendChild(isLeft ? dv.mv.connectorAlignL : dv.mv.connectorAlign),
            "d", curveAlign,
            "clip-path", "url(#glasspaneClip)",
            "class", classesDts.calign);
    }

    if (curveCorr != "") {
      //console.log('curveCorr="'+curveCorr+'"');
      attrs(svg.appendChild(isLeft ? dv.mv.connectorCorrL : dv.mv.connectorCorr),
            "d", curveCorr,
            "clip-path", "url(#glasspaneClip)",
            "class", classesDts.ccorr);
    }

  }


  function drawConnectorsForChunk(dv, chunk, sTopOrig, sTopEdit, w) {
    var flip = dv.type == "left";
    var top = dv.orig.heightAtLine(chunk.origFrom, "local", true) - sTopOrig;
    if (dv.svg) {
      var topLpx = top;
      var topRpx = dv.edit.heightAtLine(chunk.editFrom, "local", true) - sTopEdit;
      if (flip) { var tmp = topLpx; topLpx = topRpx; topRpx = tmp; }
      var botLpx = dv.orig.heightAtLine(chunk.origTo, "local", true) - sTopOrig;
      var botRpx = dv.edit.heightAtLine(chunk.editTo, "local", true) - sTopEdit;
      if (flip) { var tmp = botLpx; botLpx = botRpx; botRpx = tmp; }
      var curveTop = " C " + w/2 + " " + topRpx + " " + w/2 + " " + topLpx + " " + (w + 2) + " " + topLpx;
      var curveBot = " C " + w/2 + " " + botLpx + " " + w/2 + " " + botRpx + " -1 " + botRpx;
      attrs(dv.svg.appendChild(document.createElementNS(svgNS, "path")),
            "d", "M -1 " + topRpx + curveTop + " L " + (w + 2) + " " + botLpx + curveBot + " z",
            "class", dv.classes.connect);
    }
    if (dv.copyButtons) {
      var copy = dv.copyButtons.appendChild(elt("div", dv.type == "left" ? "\u21dd" : "\u21dc",
                                                "CodeMirror-merge-copy"));
      var editOriginals = dv.mv.options.allowEditingOriginals;
      copy.title = dv.edit.phrase(editOriginals ? "Push to left" : "Revert chunk");
      copy.chunk = chunk;
      copy.style.top = (chunk.origTo > chunk.origFrom ? top : dv.edit.heightAtLine(chunk.editFrom, "local") - sTopEdit) + "px";

      if (editOriginals) {
        var topReverse = dv.orig.heightAtLine(chunk.editFrom, "local") - sTopEdit;
        var copyReverse = dv.copyButtons.appendChild(elt("div", dv.type == "right" ? "\u21dd" : "\u21dc",
                                                         "CodeMirror-merge-copy-reverse"));
        copyReverse.title = "Push to right";
        copyReverse.chunk = {editFrom: chunk.origFrom, editTo: chunk.origTo,
                             origFrom: chunk.editFrom, origTo: chunk.editTo};
        copyReverse.style.top = topReverse + "px";
        dv.type == "right" ? copyReverse.style.left = "2px" : copyReverse.style.right = "2px";
      }
    }
  }

  function checkEdits(dv, edits) {
    var doc1 = dv.edit.getDoc();
    var doc2 = dv.orig.getDoc();
    var edit;

    var flag1 = edits.edits1 == undefined;
    var flag2 = edits.edits2 == undefined;

    //console.log('flag1='+flag1+', flag2='+flag2);

    if (flag1) edits.edits1 = [];
    if (flag2) edits.edits2 = [];

    for (var i = 0; i < edits.length; i++) {
      edit = edits[i];
      if (edit == null) continue;
      switch (edit.tag) {
      case 'DELETE':
        if (edit.line1 == undefined) {
          edit.line1 = doc1.posFromIndex(edit.start1).line;
          edit.line1_ = doc1.posFromIndex(edit.end1).line;
        }
        if (flag1) edits.edits1.push(edit);
        break;
      case 'INSERT':
        if (edit.line2 == undefined) {
          edit.line2 = doc2.posFromIndex(edit.start2).line;
          edit.line2_ = doc2.posFromIndex(edit.end2).line;
        }
        if (flag2) edits.edits2.push(edit);
        break;
      case 'RELABEL':
      case 'MOVE':
      case 'MOVREL':
        if (edit.line1 == undefined) {
          edit.line1 = doc1.posFromIndex(edit.start1).line;
          edit.line1_ = doc1.posFromIndex(edit.end1).line;
        }
        if (edit.line2 == undefined) {
          edit.line2 = doc2.posFromIndex(edit.start2).line;
          edit.line2_ = doc2.posFromIndex(edit.end2).line;
        }
        if (flag1) edits.edits1.push(edit);
        if (flag2) edits.edits2.push(edit);
        break;
      default:
        break;
      }
    }
  }
  function sortEdits(edits) {
    edits.edits1.sort(function (x, y) {
      if (x != null && y != null) {
        if (x.line1 !== undefined && y.line1 !== undefined)
          return x.line1 - y.line1;
        return 0;
      }
      return 0;
    });
    //console.log(edits.edits1);
    edits.edits2.sort(function (x, y) {
      if (x != null && y != null) {
        if (x.line2 !== undefined && y.line2 !== undefined)
          return x.line2 - y.line2;
        return 0;
      }
      return 0;
    });
    //console.log(edits.edits2);
  }


  function copyChunk(dv, to, from, chunk) {
    if (dv.diffOutOfDate) return;
    var origStart = chunk.origTo > from.lastLine() ? Pos(chunk.origFrom - 1) : Pos(chunk.origFrom, 0)
    var origEnd = Pos(chunk.origTo, 0)
    var editStart = chunk.editTo > to.lastLine() ? Pos(chunk.editFrom - 1) : Pos(chunk.editFrom, 0)
    var editEnd = Pos(chunk.editTo, 0)
    var handler = dv.mv.options.revertChunk
    if (handler)
      handler(dv.mv, from, origStart, origEnd, to, editStart, editEnd)
    else
      to.replaceRange(from.getRange(origStart, origEnd), editStart, editEnd)
  }

  // Merge view, containing 0, 1, or 2 diff views.

  var MergeView = CodeMirror.MergeView = function(node, options) {
    if (!(this instanceof MergeView)) return new MergeView(node, options);

    console.log("[diffviewer.MergeView] constructing...");

    var ess0 = options.essentialsLeft;
    var ess1 = options.essentials;
    var ess01 = options.essentialsMapped;

    if (ess0 != undefined && ess1 != undefined && ess01 != undefined) {
      this.diffDtsEss = essToDiffDts(ess0, ess1, ess01);
    }

    var svg = this.svg = document.createElementNS(svgNS, "svg");
    var glasspane = this.glasspane = elt("div", [svg], "CodeMirror-glasspane");

    this.connectorMov    = document.createElementNS(svgNS, "path");
    this.connectorRel    = document.createElementNS(svgNS, "path");
    this.connectorMovRel = document.createElementNS(svgNS, "path");
    this.connectorAlign  = document.createElementNS(svgNS, "path");
    this.connectorCorr   = document.createElementNS(svgNS, "path");

    this.rects = new Array(document.createElementNS(svgNS, "path"),
                           document.createElementNS(svgNS, "path"));

    this.defs = svg.appendChild(document.createElementNS(svgNS, "defs"));

    this.gclip = this.defs.appendChild(document.createElementNS(svgNS, "clipPath"));
    this.grect = this.gclip.appendChild(document.createElementNS(svgNS, "rect"));
    this.clip = this.defs.appendChild(document.createElementNS(svgNS, "clipPath"));
    this.rect = this.clip.appendChild(document.createElementNS(svgNS, "rect"));
    this.clipR = this.defs.appendChild(document.createElementNS(svgNS, "clipPath"));
    this.rectR = this.clipR.appendChild(document.createElementNS(svgNS, "rect"));

    this.options = options;
    var origLeft = options.origLeft
    var origRight = options.origRight == null ? options.orig : options.origRight;

    var diffLeft = options.diffLeft
    var diffRight = options.diffRight == null ? options.diff : options.diffRight;

    var hasLeft = this.hasLeft = origLeft != null;
    var hasRight = this.hasRight = origRight != null;
    var panes = this.panes = 1 + (hasLeft ? 1 : 0) + (hasRight ? 1 : 0);
    var wrap = [], left = this.left = null, right = this.right = null;
    var self = this;

    wrap.push(glasspane);

    if (hasLeft) {
      this.connectorMovL    = document.createElementNS(svgNS, "path");
      this.connectorRelL    = document.createElementNS(svgNS, "path");
      this.connectorMovRelL = document.createElementNS(svgNS, "path");
      this.connectorAlignL  = document.createElementNS(svgNS, "path");
      this.connectorCorrL   = document.createElementNS(svgNS, "path");
      this.connectorAlignL  = document.createElementNS(svgNS, "path");
      this.clipL = this.defs.appendChild(document.createElementNS(svgNS, "clipPath"));
      this.rectL = this.clipL.appendChild(document.createElementNS(svgNS, "rect"));

      left = this.left = new DiffView(this, "left", 2);
      var leftPane = elt("div", null, "CodeMirror-merge-pane CodeMirror-merge-left");
      wrap.push(leftPane);
      wrap.push(buildGap(left));
    }

    var editPane = elt("div", null, "CodeMirror-merge-pane CodeMirror-merge-editor");
    wrap.push(editPane);

    if (hasRight) {
      right = this.right = new DiffView(this, "right", 1);
      wrap.push(buildGap(right));
      var rightPane = elt("div", null, "CodeMirror-merge-pane CodeMirror-merge-right");
      wrap.push(rightPane);
    }

    (hasRight ? rightPane : editPane).className += " CodeMirror-merge-pane-rightmost";

    wrap.push(elt("div", null, null, "height: 0; clear: both;"));

    var wrapElt = this.wrap = node.appendChild(elt("div", wrap, "CodeMirror-merge CodeMirror-merge-" + panes + "pane"));

    var editOptions = copyObj(options);
    editOptions['readOnly'] = true; //"nocursor";
    this.edit = CodeMirror(editPane, editOptions);

    this.edit.on("mousedown", handleEditClick);

    if (left) left.init(leftPane, origLeft, diffLeft, options);
    if (right) right.init(rightPane, origRight, diffRight, options);

    if (left)
      this.bound = leftPane.getBoundingClientRect();
    else
      this.bound = editPane.getBoundingClientRect();

    if (left && options.lineLeft > 0)
      left.orig.scrollTo(0, left.orig.heightAtLine(options.lineLeft-1, "local"));

    if (right && options.lineRight > 0)
      right.orig.scrollTo(0, right.orig.heightAtLine(options.lineRight-1, "local"));

    if (options.line > 0)
      this.edit.scrollTo(0, this.edit.heightAtLine(options.line-1, "local"));

    if (options.collapseIdentical)
      this.editor().operation(function() {
        collapseIdenticalStretches(self, options.collapseIdentical);
      });
    if (options.connect == "align") {
      this.aligners = [];
      alignChunks(this.left || this.right, true);
    }

    if (hasLeft) {
      checkEdits(left, diffLeft);
      sortEdits(diffLeft);
    }
    if (hasRight) {
      checkEdits(right, diffRight);
      sortEdits(diffRight);
      // var i, e;
      // for (i = 0; i < diffRight.edits1.length; i++) {
      //   e = diffRight.edits1[i];
      //   console.log(1, e.tag, e.line1+'-'+e.line1_+'L', e.start1+'-'+e.end1);
      // }
      // for (i = 0; i < diffRight.edits2.length; i++) {
      //   e = diffRight.edits2[i];
      //   console.log(2, e.tag, e.line2+'-'+e.line2_+'L', e.start2+'-'+e.end2);
      // }
    }

    if (left) left.registerEvents(right);
    if (right) right.registerEvents(left);

    var onResize = function() {
      self.resetHeight();
      if (left) makeConnections(left);
      if (right) makeConnections(right);
      self.annotate();
    };
    CodeMirror.on(window, "resize", onResize);
    var resizeInterval = setInterval(function() {
      for (var p = wrapElt.parentNode; p && p != document.body; p = p.parentNode) {}
      if (!p) { clearInterval(resizeInterval); CodeMirror.off(window, "resize", onResize); }
    }, 5000);

    this.legend = document.getElementById("legend");
    //this.legend.style.visibility = "hidden";
    var delBox = document.createElement("span");
    var insBox = document.createElement("span");
    var relBox = document.createElement("span");
    var movBox = document.createElement("span");
    delBox.innerHTML = "deleted";
    attrs(delBox, "class", classesDts.del);
    insBox.innerHTML = "inserted";
    attrs(insBox, "class", classesDts.ins);
    relBox.innerHTML = "relabeled";
    attrs(relBox, "class", classesDts.rel);
    movBox.innerHTML = "moved";
    attrs(movBox, "class", classesDts.mov);
    this.legend.appendChild(delBox);
    this.legend.appendChild(document.createTextNode(" "));
    this.legend.appendChild(insBox);
    this.legend.appendChild(document.createTextNode(" "));
    this.legend.appendChild(relBox);
    this.legend.appendChild(document.createTextNode(" "));
    this.legend.appendChild(movBox);

    console.log("[diffviewer.MergeView] done.");

  };

  function buildGap(dv) {
    var lock = dv.lockButton = elt("div", null, "CodeMirror-merge-scrolllock");
    var lockWrap = elt("div", [lock], "CodeMirror-merge-scrolllock-wrap");
    CodeMirror.on(lock, "click", function() { setScrollLock(dv, !dv.lockScroll); });
    var gapElts = [lockWrap];
    if (dv.mv.options.revertButtons !== false) {
      dv.copyButtons = elt("div", null, "CodeMirror-merge-copybuttons-" + dv.type);
      CodeMirror.on(dv.copyButtons, "click", function(e) {
        var node = e.target || e.srcElement;
        if (!node.chunk) return;
        if (node.className == "CodeMirror-merge-copy-reverse") {
          copyChunk(dv, dv.orig, dv.edit, node.chunk);
          return;
        }
        copyChunk(dv, dv.edit, dv.orig, node.chunk);
      });
      gapElts.unshift(dv.copyButtons);
    }
    if (dv.mv.options.connect != "align") {
      var svg = document.createElementNS && document.createElementNS(svgNS, "svg");
      if (svg && !svg.createSVGRect) svg = null;
      dv.svg = svg;
      if (svg) gapElts.push(svg);
    }

    return dv.gap = elt("div", gapElts, "CodeMirror-merge-gap");
  }

  MergeView.prototype = {
    constuctor: MergeView,
    editor: function() { return this.edit; },
    rightOriginal: function() { return this.right && this.right.orig; },
    leftOriginal: function() { return this.left && this.left.orig; },
    setShowDifferences: function(val) {
      if (this.right) this.right.setShowDifferences(val);
      if (this.left) this.left.setShowDifferences(val);
    },
    rightChunks: function() {
      if (this.right) { ensureDiff(this.right); return this.right.chunks; }
    },
    leftChunks: function() {
      if (this.left) { ensureDiff(this.left); return this.left.chunks; }
    },
    setShowDts: function(val) {
      //console.log('[setShowDts]', val);

      if (this.right) this.right.setShowDts(val);
      if (this.left) this.left.setShowDts(val);

      if (val == 3 && this.left && this.right) {
        clear(this.svg);
        if (this.hasRight) makeConnections(this.right);
        if (this.hasLeft) makeConnections(this.left);
      }

      this.annotate();

      if (val) {
        this.legend.style.visibility = "visible";
      } else {
        this.legend.style.visibility = "hidden";
      }
    },
    init: function() {
      this.resetHeight();
      this.annotate();
    },
    annotate: function() {
      if (this.left) {
        //console.log("[annotate:left] showDts="+this.left.showDts);
        var dv = this.left;
        if ((dv.showDts & dv.index) > 0) {
          if (!dv.dtsAnnot) {
            dv.dtsAnnot = new DtsScrollbarAnnotation(dv.edit, dv.orig, dv.diffDts);
          } else {
            try {
              dv.dtsAnnot.clear();
            } catch (exn) {
              dv.dtsAnnot = new DtsScrollbarAnnotation(dv.edit, dv.orig, dv.diffDts);
            }
          }
          dv.dtsAnnot.mark();
        } else if (this.diffDtsEss != []) {
          if (!dv.dtsAnnotEss) {
            dv.dtsAnnotEss = new DtsScrollbarAnnotation(dv.edit, dv.orig, this.diffDtsEss);
          } else {
            try {
              dv.dtsAnnotEss.clear();
            } catch (exn) {
              dv.dtsAnnotEss = new DtsScrollbarAnnotation(dv.edit, dv.orig, this.diffDtsEss);
            }
          }
          dv.dtsAnnotEss.mark();
        }
      }
      if (this.right) {
        //console.log("[annotate:right] showDts="+this.right.showDts);
        var dv = this.right;
        if ((dv.showDts & dv.index) > 0) {
          if (!dv.dtsAnnot) {
            dv.dtsAnnot = new DtsScrollbarAnnotation(dv.edit, dv.orig, dv.diffDts);
          } else {
            try {
              dv.dtsAnnot.clear();
            } catch (exn) {
              dv.dtsAnnot = new DtsScrollbarAnnotation(dv.edit, dv.orig, dv.diffDts);
            }
          }
          dv.dtsAnnot.mark();
        }
      }
    },
    resetHeight: function() {
      var wh = $(window).height();
      var head = document.getElementById("head");
      var tail = document.getElementById("tail");
      var h = (wh-head.offsetHeight-tail.offsetHeight)+"px";

      //console.log("wh="+wh+" h="+h);

      var cmms = document.getElementsByClassName("CodeMirror-merge");
      for (i = 0; i < cmms.length; i++) {
        cmms[i].style.height = h;
      }
      var cms = document.getElementsByClassName("CodeMirror");
      for (i = 0; i < cms.length; i++) {
        cms[i].style.height = h;
      }
    }
  };

  function asString(obj) {
    if (typeof obj == "string") return obj;
    else return obj.getValue();
  }

  // Operations on diffs
  var dmp;
  function getDiff(a, b, ignoreWhitespace) {
    if (!dmp) dmp = new diff_match_patch();
      var diff = dmp.diff_main(a, b);
      // The library sometimes leaves in empty parts, which confuse the algorithm
      for (var i = 0; i < diff.length; ++i) {
        var part = diff[i];
        if (ignoreWhitespace ? !/[^ \t]/.test(part[1]) : !part[1]) {
          diff.splice(i--, 1);
        } else if (i && diff[i - 1][0] == part[0]) {
          diff.splice(i--, 1);
          diff[i][1] += part[1];
        }
      }
      return diff;
  }

  function iterateChunksDts(diffDts, f) {
    for (var i = 0; i < diffDts.length; ++i) {
      var part = diffDts[i];
      if (part != null) {
        var tp = part["tag"];
        if (tp == "ALIGN") {
          var line1 = part["line1"];
          var line2 = part["line2"];
          var ext = part["ext"];
          f(line1, line1+ext-1, line2, line2+ext-1);
        }
      }
    }
  }

  function getChunks(diff) {
    var chunks = [];
    if (!diff.length) return chunks;
    var startEdit = 0, startOrig = 0;
    var edit = Pos(0, 0), orig = Pos(0, 0);
    for (var i = 0; i < diff.length; ++i) {
      var part = diff[i], tp = part[0];
      if (tp == DIFF_EQUAL) {
        var startOff = !startOfLineClean(diff, i) || edit.line < startEdit || orig.line < startOrig ? 1 : 0;
        var cleanFromEdit = edit.line + startOff, cleanFromOrig = orig.line + startOff;
        moveOver(edit, part[1], null, orig);
        var endOff = endOfLineClean(diff, i) ? 1 : 0;
        var cleanToEdit = edit.line + endOff, cleanToOrig = orig.line + endOff;
        if (cleanToEdit > cleanFromEdit) {
          if (i) chunks.push({origFrom: startOrig, origTo: cleanFromOrig,
                              editFrom: startEdit, editTo: cleanFromEdit});
          startEdit = cleanToEdit; startOrig = cleanToOrig;
        }
      } else {
        moveOver(tp == DIFF_INSERT ? edit : orig, part[1]);
      }
    }
    if (startEdit <= edit.line || startOrig <= orig.line)
      chunks.push({origFrom: startOrig, origTo: orig.line + 1,
                   editFrom: startEdit, editTo: edit.line + 1});
    return chunks;
  }

  function endOfLineClean(diff, i) {
    if (i == diff.length - 1) return true;
    var next = diff[i + 1][1];
    if ((next.length == 1 && i < diff.length - 2) || next.charCodeAt(0) != 10) return false;
    if (i == diff.length - 2) return true;
    next = diff[i + 2][1];
    return (next.length > 1 || i == diff.length - 3) && next.charCodeAt(0) == 10;
  }

  function startOfLineClean(diff, i) {
    if (i == 0) return true;
    var last = diff[i - 1][1];
    if (last.charCodeAt(last.length - 1) != 10) return false;
    if (i == 1) return true;
    last = diff[i - 2][1];
    return last.charCodeAt(last.length - 1) == 10;
  }

  function chunkBoundariesAround(chunks, n, nInEdit) {
    var beforeE, afterE, beforeO, afterO;
    for (var i = 0; i < chunks.length; i++) {
      var chunk = chunks[i];
      var fromLocal = nInEdit ? chunk.editFrom : chunk.origFrom;
      var toLocal = nInEdit ? chunk.editTo : chunk.origTo;
      if (afterE == null) {
        if (fromLocal > n) { afterE = chunk.editFrom; afterO = chunk.origFrom; }
        else if (toLocal > n) { afterE = chunk.editTo; afterO = chunk.origTo; }
      }
      if (toLocal <= n) { beforeE = chunk.editTo; beforeO = chunk.origTo; }
      else if (fromLocal <= n) { beforeE = chunk.editFrom; beforeO = chunk.origFrom; }
    }
    return {edit: {before: beforeE, after: afterE}, orig: {before: beforeO, after: afterO}};
  }

  function chunkBoundariesAroundDts(diffDts, n, nInEdit) {
    var beforeE, afterE, beforeO, afterO;
    iterateChunksDts(diffDts, function(fromOrig, toOrig, fromEdit, toEdit) {
      var fromLocal = nInEdit ? fromEdit : fromOrig;
      var toLocal = nInEdit ? toEdit : toOrig;
      if (afterE == null) {
        if (fromLocal > n) { afterE = fromEdit; afterO = fromOrig; }
        else if (toLocal > n) { afterE = toEdit; afterO = toOrig; }
      }
      if (toLocal <= n) { beforeE = toEdit; beforeO = toOrig; }
      else if (fromLocal <= n) { beforeE = fromEdit; beforeO = fromOrig; }
    });
    return {edit: {before: beforeE, after: afterE}, orig: {before: beforeO, after: afterO}};
  }

  function collapseSingle(cm, from, to) {
    cm.addLineClass(from, "wrap", "CodeMirror-merge-collapsed-line");
    var widget = document.createElement("span");
    widget.className = "CodeMirror-merge-collapsed-widget";
    widget.title = cm.phrase("Identical text collapsed. Click to expand.");
    var mark = cm.markText(Pos(from, 0), Pos(to - 1), {
      inclusiveLeft: true,
      inclusiveRight: true,
      replacedWith: widget,
      clearOnEnter: true
    });
    function clear() {
      mark.clear();
      cm.removeLineClass(from, "wrap", "CodeMirror-merge-collapsed-line");
    }
    if (mark.explicitlyCleared) clear();
    CodeMirror.on(widget, "click", clear);
    mark.on("clear", clear);
    CodeMirror.on(widget, "click", clear);
    return {mark: mark, clear: clear};
  }

  function collapseStretch(size, editors) {
    var marks = [];
    function clear() {
      for (var i = 0; i < marks.length; i++) marks[i].clear();
    }
    for (var i = 0; i < editors.length; i++) {
      var editor = editors[i];
      var mark = collapseSingle(editor.cm, editor.line, editor.line + size);
      marks.push(mark);
      mark.mark.on("clear", clear);
    }
    return marks[0].mark;
  }

  function unclearNearChunks(dv, margin, off, clear) {
    for (var i = 0; i < dv.chunks.length; i++) {
      var chunk = dv.chunks[i];
      for (var l = chunk.editFrom - margin; l < chunk.editTo + margin; l++) {
        var pos = l + off;
        if (pos >= 0 && pos < clear.length) clear[pos] = false;
      }
    }
  }

  function collapseIdenticalStretches(mv, margin) {
    if (typeof margin != "number") margin = 2;
    var clear = [], edit = mv.editor(), off = edit.firstLine();
    for (var l = off, e = edit.lastLine(); l <= e; l++) clear.push(true);
    if (mv.left) unclearNearChunks(mv.left, margin, off, clear);
    if (mv.right) unclearNearChunks(mv.right, margin, off, clear);

    for (var i = 0; i < clear.length; i++) {
      if (clear[i]) {
        var line = i + off;
        for (var size = 1; i < clear.length - 1 && clear[i + 1]; i++, size++) {}
        if (size > margin) {
          var editors = [{line: line, cm: edit}];
          if (mv.left) editors.push({line: getMatchingOrigLine(line, mv.left.chunks), cm: mv.left.orig});
          if (mv.right) editors.push({line: getMatchingOrigLine(line, mv.right.chunks), cm: mv.right.orig});
          var mark = collapseStretch(size, editors);
          if (mv.options.onCollapse) mv.options.onCollapse(mv, line, size, mark);
        }
      }
    }
  }

  // General utilities

  function elt(tag, content, className, style) {
    var e = document.createElement(tag);
    if (className) e.className = className;
    if (style) e.style.cssText = style;
    if (typeof content == "string") e.appendChild(document.createTextNode(content));
    else if (content) for (var i = 0; i < content.length; ++i) e.appendChild(content[i]);
    return e;
  }

  function clear(node) {
    for (var count = node.childNodes.length; count > 0; --count)
      node.removeChild(node.firstChild);
  }

  function attrs(elt) {
    for (var i = 1; i < arguments.length; i += 2)
      elt.setAttribute(arguments[i], arguments[i+1]);
  }

  function copyObj(obj, target) {
    if (!target) target = {};
    for (var prop in obj) if (obj.hasOwnProperty(prop)) target[prop] = obj[prop];
    return target;
  }

  function moveOver(pos, str, copy, other) {
    var out = copy ? Pos(pos.line, pos.ch) : pos, at = 0;
    for (;;) {
      var nl = str.indexOf("\n", at);
      if (nl == -1) break;
      ++out.line;
      if (other) ++other.line;
      at = nl + 1;
    }
    out.ch = (at ? 0 : out.ch) + (str.length - at);
    if (other) other.ch = (at ? 0 : other.ch) + (str.length - at);
    return out;
  }

  // Tracks collapsed markers and line widgets, in order to be able to
  // accurately align the content of two editors.

  var F_WIDGET = 1, F_WIDGET_BELOW = 2, F_MARKER = 4

  function TrackAlignable(cm) {
    this.cm = cm
    this.alignable = []
    this.height = cm.doc.height
    var self = this
    cm.on("markerAdded", function(_, marker) {
      if (!marker.collapsed) return
      var found = marker.find(1)
      if (found != null) self.set(found.line, F_MARKER)
    })
    cm.on("markerCleared", function(_, marker, _min, max) {
      if (max != null && marker.collapsed)
        self.check(max, F_MARKER, self.hasMarker)
    })
    cm.on("markerChanged", this.signal.bind(this))
    cm.on("lineWidgetAdded", function(_, widget, lineNo) {
      if (widget.mergeSpacer) return
      if (widget.above) self.set(lineNo - 1, F_WIDGET_BELOW)
      else self.set(lineNo, F_WIDGET)
    })
    cm.on("lineWidgetCleared", function(_, widget, lineNo) {
      if (widget.mergeSpacer) return
      if (widget.above) self.check(lineNo - 1, F_WIDGET_BELOW, self.hasWidgetBelow)
      else self.check(lineNo, F_WIDGET, self.hasWidget)
    })
    cm.on("lineWidgetChanged", this.signal.bind(this))
    cm.on("change", function(_, change) {
      var start = change.from.line, nBefore = change.to.line - change.from.line
      var nAfter = change.text.length - 1, end = start + nAfter
      if (nBefore || nAfter) self.map(start, nBefore, nAfter)
      self.check(end, F_MARKER, self.hasMarker)
      if (nBefore || nAfter) self.check(change.from.line, F_MARKER, self.hasMarker)
    })
    cm.on("viewportChange", function() {
      if (self.cm.doc.height != self.height) self.signal()
    })
  }

  TrackAlignable.prototype = {
    signal: function() {
      CodeMirror.signal(this, "realign")
      this.height = this.cm.doc.height
    },

    set: function(n, flags) {
      var pos = -1
      for (; pos < this.alignable.length; pos += 2) {
        var diff = this.alignable[pos] - n
        if (diff == 0) {
          if ((this.alignable[pos + 1] & flags) == flags) return
          this.alignable[pos + 1] |= flags
          this.signal()
          return
        }
        if (diff > 0) break
      }
      this.signal()
      this.alignable.splice(pos, 0, n, flags)
    },

    find: function(n) {
      for (var i = 0; i < this.alignable.length; i += 2)
        if (this.alignable[i] == n) return i
      return -1
    },

    check: function(n, flag, pred) {
      var found = this.find(n)
      if (found == -1 || !(this.alignable[found + 1] & flag)) return
      if (!pred.call(this, n)) {
        this.signal()
        var flags = this.alignable[found + 1] & ~flag
        if (flags) this.alignable[found + 1] = flags
        else this.alignable.splice(found, 2)
      }
    },

    hasMarker: function(n) {
      var handle = this.cm.getLineHandle(n)
      if (handle.markedSpans) for (var i = 0; i < handle.markedSpans.length; i++)
        if (handle.markedSpans[i].marker.collapsed && handle.markedSpans[i].to != null)
          return true
      return false
    },

    hasWidget: function(n) {
      var handle = this.cm.getLineHandle(n)
      if (handle.widgets) for (var i = 0; i < handle.widgets.length; i++)
        if (!handle.widgets[i].above && !handle.widgets[i].mergeSpacer) return true
      return false
    },

    hasWidgetBelow: function(n) {
      if (n == this.cm.lastLine()) return false
      var handle = this.cm.getLineHandle(n + 1)
      if (handle.widgets) for (var i = 0; i < handle.widgets.length; i++)
        if (handle.widgets[i].above && !handle.widgets[i].mergeSpacer) return true
      return false
    },

    map: function(from, nBefore, nAfter) {
      var diff = nAfter - nBefore, to = from + nBefore, widgetFrom = -1, widgetTo = -1
      for (var i = 0; i < this.alignable.length; i += 2) {
        var n = this.alignable[i]
        if (n == from && (this.alignable[i + 1] & F_WIDGET_BELOW)) widgetFrom = i
        if (n == to && (this.alignable[i + 1] & F_WIDGET_BELOW)) widgetTo = i
        if (n <= from) continue
        else if (n < to) this.alignable.splice(i--, 2)
        else this.alignable[i] += diff
      }
      if (widgetFrom > -1) {
        var flags = this.alignable[widgetFrom + 1]
        if (flags == F_WIDGET_BELOW) this.alignable.splice(widgetFrom, 2)
        else this.alignable[widgetFrom + 1] = flags & ~F_WIDGET_BELOW
      }
      if (widgetTo > -1 && nAfter)
        this.set(from + nAfter, F_WIDGET_BELOW)
    }
  }

  function posMin(a, b) { return (a.line - b.line || a.ch - b.ch) < 0 ? a : b; }
  function posMax(a, b) { return (a.line - b.line || a.ch - b.ch) > 0 ? a : b; }
  function posEq(a, b) { return a.line == b.line && a.ch == b.ch; }

  function findPrevDiff(chunks, start, isOrig) {
    for (var i = chunks.length - 1; i >= 0; i--) {
      var chunk = chunks[i];
      var to = (isOrig ? chunk.origTo : chunk.editTo) - 1;
      if (to < start) return to;
    }
  }

  function findNextDiff(chunks, start, isOrig) {
    for (var i = 0; i < chunks.length; i++) {
      var chunk = chunks[i];
      var from = (isOrig ? chunk.origFrom : chunk.editFrom);
      if (from > start) return from;
    }
  }

  function goNearbyDiff(cm, dir) {
    var found = null, views = cm.state.diffViews, line = cm.getCursor().line;
    if (views) for (var i = 0; i < views.length; i++) {
      var dv = views[i], isOrig = cm == dv.orig;
      ensureDiff(dv);
      var pos = dir < 0 ? findPrevDiff(dv.chunks, line, isOrig) : findNextDiff(dv.chunks, line, isOrig);
      if (pos != null && (found == null || (dir < 0 ? pos > found : pos < found)))
        found = pos;
    }
    if (found != null)
      cm.setCursor(found, 0);
    else
      return CodeMirror.Pass;
  }

  CodeMirror.commands.goNextDiff = function(cm) {
    return goNearbyDiff(cm, 1);
  };
  CodeMirror.commands.goPrevDiff = function(cm) {
    return goNearbyDiff(cm, -1);
  };

});
