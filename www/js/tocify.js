// `tocify.js`
// Minimalist Table of Contents generator for Markdown generated
// HTML documents.
// Developed by Romain Gaucher (@rgaucher), March 2013
(function(){
  var toc_root = "#toc";
  var toc_elmt = $(toc_root);
  var doc_anchor_char = "&#xb6;";
  var toc_max_depth = 3;
  var toc_document_root = "#document-container";
  //var toc_document_root_headers = toc_document_root + " > article > :header";
  var toc_document_root_headers = toc_document_root + " :header";
  

  if (toc_elmt === undefined) {
    console.log("The `toc_root` element must exist at this point...");
  }

  String.prototype.slugify = function() {
    return this.replace(/[^-a-zA-Z0-9,&\s]+/ig, '')
               .replace(/-/gi, "_")
               .replace(/\s/gi, "-")
               .toLowerCase();  
  }

  String.prototype.selectorEscape = function() {
      return this.replace(/[!"#$%&'()*+,.\/:;<=>?@\[\\\]^`{|}~]/g, 
                          '\\$&');
  }

  // We assume lots of stuff here. For now, for instance, this
  // only works with simple heading elements:
  //  <h1>Just text here</h1>
  // the nested HTML will be ignored.
  function extractTitle(elmt, level) {
    return $(elmt).text();
  }


  function appendSlugishAnchor(elmt, slug) {
    $(elmt)
    .prepend(
      $("<a />")
      .attr("name", slug)
      .text("")
    )
    .append(
      $("<a />")
      .attr("href", "#" + slug)
      .attr("class", "toc-anchor-char")
      .html(doc_anchor_char)
    )
  }


  function extractIndentLevel(headerTagName) {
    if (/^h(\d)$/i.test(headerTagName)) {
      return parseInt(headerTagName.charAt(1));
    }
    return 1; // Default ident level is top-level
  }


  function appendTocElmt(current_title, current_slug, parent_elmt) {
    parent_elmt.append(
      $("<li />")
      .html(
        $("<a />")
        .attr("href", "#" + current_slug)
        .text(current_title)
      )
      .prepend('<i class="icon-minus"></i>')
      .append(
        $("<ol />")
        .attr('slug-sel', current_slug)
        .text("")
      )
    )
    return $("ol[slug-sel='" + current_slug.selectorEscape() + "']");
  }


  function retrieveHeaders(root_elmt, root_toc) {
    var indent_level = 1;
    var cur_elmt = null;
    var sized_stack = {
      0 : root_toc
    };

    $(toc_document_root_headers).each(function(index) {
      indent_level = extractIndentLevel(this.nodeName);

      if (indent_level > toc_max_depth)
        return true;

      if (!sized_stack.hasOwnProperty(indent_level)) {
        sized_stack[indent_level] = null;
      }

      // Extract info, append anchors
      current_title = extractTitle(this);
      current_slug = current_title.slugify();
      appendSlugishAnchor(this, current_slug);

      // Parent element to which we append a new toc-entry
      parent_elmt = sized_stack[indent_level - 1];
      if (parent_elmt !== null) {
        cur_elmt = appendTocElmt(current_title, current_slug, parent_elmt);

        // Register our latest elmt for the stack
        sized_stack[indent_level] = cur_elmt;
      }
    });
  }


  function getDocumentRoot() {
    return $(toc_document_root);
  }


  function getRootElmt() {
    toc_elmt.append(
      $("<ol />")
      .attr("id", "toc-top-level")
    );
    return $("#toc-top-level");
  }

  retrieveHeaders(getDocumentRoot(), 
                  getRootElmt());

})();