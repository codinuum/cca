
function DiffViewer() {
  this.diff = false;
  this.dts = 0;
}

function handleError(xhr, mes, err) {
  console.log("[dv.DiffViewer] "+mes+" ("+err+")");
}

/*DiffViewer.prototype.show2 = function(original, modified, diff, elem, mode, line1, line2, ess1, ess2, ess12) {
  var self = this;

  console.log("[dv.DiffViewer] mode="+mode);

  self.mv = CodeMirror.MergeView(elem,{
    revertButtons:false,
    diffRight:diff,
    value:original,
    origRight:modified,
    mode:mode,
    scrollbarStyle:"simple",
    foldGutter:true,
    gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter"],
    lineNumbers:true,
    lineSeparator:'\n',
    line:line2,
    lineLeft:line1,
    essentials:ess2,
    essentialsLeft:ess1,
    essentialsMapped:ess12,
    showDifferences:self.diff,
    showDts:self.dts
  });

  self.mv.init();

}*/

DiffViewer.prototype.show = function(url0, url1, urld, elem, mode, line1, line2, ess1, ess2, ess12,
                                    blob0, blob1) {
  var self = this;

  console.log("[dv.DiffViewer] mode="+mode);

  console.log("[dv.DiffViewer] loading files...");

  console.log("[dv.DiffViewer] url0="+url0);
  console.log("[dv.DiffViewer] url1="+url1);
  console.log("[dv.DiffViewer] urld="+urld);

  if (blob0) {
    const original = blob0;
    const modified = blob1;
    $.ajax({
      type:"GET",
      mimeType:"application/json",
      url:urld,
      error:handleError
    }).done(function(diff, status2, xhr2) {
      console.log("[dv.DiffViewer] diff file loaded.");

      self.mv = CodeMirror.MergeView(elem,{
        revertButtons:false,
        diffRight:diff,
        value:original,
        origRight:modified,
        mode:mode,
        scrollbarStyle:"simple",
        foldGutter:true,
        gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter"],
        lineNumbers:true,
        lineSeparator:'\n',
        line:line1,
        lineRight:line2,
        essentials:ess2,
        essentialsLeft:ess1,
        essentialsMapped:ess12,
        showDifferences:self.diff,
        showDts:self.dts
      });

      self.mv.init();
    })

  } else {
    $.ajax({
      type:"GET",
      mimeType:"text/plain",
      url:url0,
      error:handleError
    }).done(function(original, status0, xhr0) {
      console.log("[dv.DiffViewer] original source file loaded.");
      $.ajax({
        type:"GET",
        mimeType:"text/plain",
        url:url1,
        error:handleError
      }).done(function(modified, status1, xhr1) {
        console.log("[dv.DiffViewer] modified source file loaded.");
        $.ajax({
          type:"GET",
          mimeType:"application/json",
          url:urld,
          error:handleError
        }).done(function(diff, status2, xhr2) {
          console.log("[dv.DiffViewer] diff file loaded.");

          self.mv = CodeMirror.MergeView(elem,{
            revertButtons:false,
            diffRight:diff,
            value:original,
            origRight:modified,
            mode:mode,
            scrollbarStyle:"simple",
            foldGutter:true,
            gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter"],
            lineNumbers:true,
            lineSeparator:'\n',
            line:line1,
            lineRight:line2,
            essentials:ess2,
            essentialsLeft:ess1,
            essentialsMapped:ess12,
            showDifferences:self.diff,
            showDts:self.dts
          });

          self.mv.init();
        })
      })
    })
  }

}

DiffViewer.prototype.toggleDifferences = function() {
  this.mv.setShowDifferences(this.diff = !this.diff);
}

DiffViewer.prototype.toggleDts = function() {
  this.mv.setShowDts(this.dts = this.dts > 0 ? 0 : 1);
}

