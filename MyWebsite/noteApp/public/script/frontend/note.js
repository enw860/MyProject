var isLogedIn = false;

//check session expired state every 5 seconds
function check_session_expired(){
	var seconds = 5;
	var url = origin + "/api/users/session_expired";

	$.ajax({
		type:"GET",
	    url: url,
	    success: function(data){
	    	setTimeout(function(){
	    		if(data){
	    			isLogedIn = false;
	    			logoutAction();
		    		alert("session_expired");
		    	}else{
		    		isLogedIn = true;
		    		check_session_expired();
		    	}
	    	},seconds * 1000);
	    }, error: function(){
	    	logoutAction();
	    	console.log("abnormal network error");
	    },
	})
}

//determine if user is logged in already
function showNote(){
	var dataURL = origin+"/api/users/data";

	$.ajax({
		type: "GET",
		url: dataURL,
		success: function(data){
			if(data!==false){
				$("#Entry").hide();
				$("#note").show();
				$("#userInfo").show();
				makeNote(data);
				if(isLogedIn==false){
					check_session_expired();
				}
			}else{
				try {
					$("#note").hide();
					$("#Entry").show();
					$("#userInfo").hide();
					showLogIn();
				} catch (e) {}
			}
		}
	});
}

function makeNote(userData){
	userEmail.innerHTML = userData.email;
	ReactDOM.render(React.createElement(Note, {userData : userData}), document.getElementById('note'));
}

function logoutAction(){
	userEmail.innerHTML = "";
	window.location.replace("/users/logout");
}

function clickSave(){
	$("#save_button").click();
}

showNote();
