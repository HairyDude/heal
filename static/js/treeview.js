// From http://www.gnostice.com/nl_article.asp?id=208&t=Interactive_Tree_Control_Using_An_HTML_List_and_JavaScript

// Add this to the onload event of the BODY element
function addEvents() {
    // Get ULs that we want to collapse
    // Note that you only need to put the LinkedList class on the root of each
    // tree for this to work.
    var trees = document.getElementsByClassName("LinkedList");
    for (i=0; i<trees.length; i++) {
        activateTree(trees[i]);
    }
}

// This function traverses the list and add links
// to nested list items
function activateTree(oList) {
    // Get all LIs in this list (recursively)
    var branches = oList.getElementsByTagName("li");

    for (var i=0; i < branches.length; i++) {
        if (branches[i].getElementsByTagName("ul").length > 0) {
            // item has sublists
            branches[i].className += "closed";
        } else {
            // item doesn't have any sublists
            branches[i].className += "open empty";
        }
    }

    // Add the click-event handler to the list items
    if (oList.addEventListener) {
        oList.addEventListener("click", toggleBranch, false);
    } else if (oList.attachEvent) { // For IE
        oList.attachEvent("onclick", toggleBranch);
    }
}

// This is the click-event handler
function toggleBranch(event) {
    var oBranch;
    var cSubBranches;
    if (event.target) {
        oBranch = event.target;
    } else if (event.srcElement) { // For IE
        oBranch = event.srcElement;
    }

    cSubBranches = oBranch.getElementsByTagName("ul");

    if (oBranch.className.indexOf("empty") > -1) {
        // don't toggle
    }
    else if (oBranch.className.indexOf("closed") > -1) {
        changeClass(oBranch, "closed", "open");
    } else {
        changeClass(oBranch, "open", "closed");
    }

    if (cSubBranches.length > 0) {
        // closed by default, see activateTree() above
        if (cSubBranches[0].className.indexOf("closed") > -1) {
            changeClass(cSubBranches[0], "closed", "open");
        } else {
            changeClass(cSubBranches[0], "open", "closed");
        }
    }
}

// Replace a class
// TODO: add this to the element prototype
function changeClass(elem, oldclass, newclass) {
    elem.className = elem.className.replace(oldclass, newclass);
}
