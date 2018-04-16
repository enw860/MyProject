function buildEntryStructure(){
	$("#Entry").append("<div id=\"EntryContent\"></div>");

	$("#EntryContent")
		.append("<div id=\"EntryTabs\"></div>")
		.append("<div id=\"EntryBody\"></div>");

	$("#EntryTabs")
		.append("<div id=\"Tabs\"></div>");

	$("#EntryBody")
	    .append("<div id=\"content\"></div>")

	$("#Tabs")	
		.append("<button id=\"sig\" onclick=\"showLogIn()\">Sign In</li>")
  		.append("<button id=\"reg\" onclick=\"showRegister()\">Create Account</li>");
}

function addRegister(){
	ReactDOM.render(React.createElement(RegisteNew, null), document.getElementById('content'));
}

function addLogIn(){
	ReactDOM.render(React.createElement(UserLogIn, null), document.getElementById('content'));
}

function showRegister(){
	ReactDOM.unmountComponentAtNode(document.getElementById("content"));
	$("#sig").addClass("active");
	$("#reg").removeClass("active");
	addRegister();
}

function showLogIn(){
	ReactDOM.unmountComponentAtNode(document.getElementById("content"));
	$("#reg").addClass("active");
	$("#sig").removeClass("active");
	addLogIn();
}

buildEntryStructure();
