/*
 * This JavaScript file is used by browse_results.jsp, search_results.jsp, details.jsp, and tabs.jsp
 */

function viewDetails(docId, hasMap) {
	top.setCurrentContent(docId);
	
	if (hasMap) {
		top.tabs.location.replace("tabs.jsp?details=active&map=visible");
	} else {
		top.tabs.location.replace("tabs.jsp?details=active");
	}
	
	top.contentFrames.location.href = "content_frames.jsp?goTo=details&docId=" + docId + "&mode=" + top.getMode() + "&loggedIn=" + top.loggedIn;
}

function viewMap(docId) {
	var mapURL = top.getMapURL();
	
	top.setCurrentContent(docId);
	
	top.tabs.location.replace("tabs.jsp?map=active&details=visible");

	// Get the saved map url's docId
	var mapDocId = "";
	var index = mapURL.indexOf("docId=");
	if (index != -1) {
		var index2 = mapURL.indexOf("&", index);
		if (index2 != -1)
			mapDocId = mapURL.substring(index + 6, index2);
		else
			mapDocId = mapURL.substring(index + 6);
	}

	// load the map, or if returning to the map from Details, load the last view
	if (mapURL == "" || unescape(docId) != unescape(mapDocId)) { // saved map url is blank or for a different map service
		top.cleanViewHistory();
		top.setCurrentHistoryPosition(0);
		
		mapURL = "content_frames.jsp?goTo=map&docId=" + docId + "&loggedIn=" + top.loggedIn;
		
		var searchURL = top.getSearchURL();
		if (searchURL.length > 0) {
			var x1 = getKey(searchURL, "&west=");
			var x2 = getKey(searchURL, "&east=");
			var y1 = getKey(searchURL, "&south=");
			var y2 = getKey(searchURL, "&north=");
			mapURL += "&x1=" + x1 + "&y1=" + y1 + "&x2=" + x2 + "&y2=" + y2;
		}
		
		top.contentFrames.location.href = mapURL;
	} else {
		index = mapURL.indexOf("?");
		mapURL = mapURL.substring(index); // get parameters
		
		if (mapURL.indexOf("goTo=") == -1) // add goTo=map
			mapURL += "&goTo=map";
		
		// remove old w and h parameters
		mapURL = removeKey(mapURL, "&w=");
		mapURL = removeKey(mapURL, "&h=");

		top.isBackForwardMove = true; // don't add to history
		top.contentFrames.location.href = "content_frames.jsp" + mapURL; // use saved map url
	}
}

function addToArcExplorer(docId, theGnd) {
	var url = "arcexplorer_applet.jsp?gndUrl=" + escape(theGnd) + "&mode=" + top.getMode() + "&port=" + top.ArcExplorerPort;
	top.hiddenFrame.location.href = url;
}

function removeKey(string, key) {
	if (string.indexOf(key) != -1) {
		var start = string.indexOf(key);
		var end = string.indexOf("&", start + 2);
		if (end == -1)
			string = string.substring(0, start);
		else			
			string = string.substring(0, start) + string.substring(end);
	}
	return string;
}

function getKey(string, key) {
	var value = "";
	if (string.indexOf(key) != -1) {
		var start = string.indexOf(key) + key.length;
		var end = string.indexOf("&", start + 2);
		if (end == -1)
			value = string.substring(start);
		else
			value = string.substring(start, end);
	}
	return value;
}

function jumpToBatch(batchNumber, resultNumber, pageType) {
	var goToURL = "";
	var resultFramesPage = "";

	if (pageType == 'browse')	{
		goToURL = top.getBrowseURL(); // get last browse URL
		resultFramesPage = "browse_result_frames.jsp";
	} else { // pageType is search
		goToURL = top.getSearchURL(); // get last search URL
		resultFramesPage = "search_result_frames.jsp";
	}
	
	var index = goToURL.indexOf("?");
	goToURL = goToURL.substring(index) // get just the parameters of the saved URL
		
	//remove the parameter 'batchtoStartAt' if there is one
	goToURL = removeKey(goToURL, "&startBatchAt=");
	
	// add parameter of which batch to jump to (the starting result of the batch) 
	goToURL = goToURL + "&startBatchAt=" + resultNumber;

	// do the jump
	top.contentFrames.resultFrames.location.href = resultFramesPage + goToURL;
}