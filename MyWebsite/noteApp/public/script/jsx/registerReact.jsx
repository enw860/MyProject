class RegisteNew extends React.Component{
	constructor(props){
		super(props);
		this.state = {
			email: '',
			password: '',
			checkPassowrd: ''
		};

		this.handelSubmit = this.handelSubmit.bind(this);
		this.gotoLogIn = this.gotoLogIn.bind(this);

		this.emailOnChange = this.emailOnChange.bind(this);
		this.passwordOnChange = this.passwordOnChange.bind(this);
		this.checkPasswordOnChange = this.checkPasswordOnChange.bind(this);
		
	}

	handelSubmit(e){
		e.preventDefault();

		if((this.state.password=='')||(this.state.email=='')){
			reg_err_message.innerHTML = "have to have a email and a password";
			return;
		}

		var newUser = this.state;
		delete newUser.checkPassowrd; 

		var url = window.location.origin + "/api/users";
		$.ajax({
	        type:"POST",
	        contentType: 'application/json',
	        dataType: 'json',
	        url:url,
	        data: JSON.stringify(newUser),
	        success: function (data) {
	        	if(data!==false){
	        		$("#reg_err_message").show();
	        		$("#reg_err_message").text("Success");
	        		setTimeout(function(){
	        			showLogIn();
	        		},2000);
	        	}else{
	        		reg_err_message.innerHTML = "This email already exists";
	        		setTimeout(function(){
						reg_err_message.innerHTML = "";
	        		},3000)
	        	}
	        },error:function(){
	        	console.log("error");
	        }
	    });
	}

	gotoLogIn(e){
		showLogIn();
	}

	emailOnChange(e){
		var email = e.target.value;
		this.setState({email: email});
		if(email==''){
			reg_err_message.innerHTML = "email cannot empty";
			$("#create-account").prop('disabled', true);
		}else{
			reg_err_message.innerHTML = "";
		}
	}

	passwordOnChange(e){
		var password = e.target.value;
		this.setState({password: password});
		$("#create-account").prop('disabled', true);
		if(password==''){
			reg_err_message.innerHTML = "password cannot empty";
		}else{
			reg_err_message.innerHTML = "";
			if (checkConfirm.innerHTML == ""){
				$("#create-account").prop('disabled', false);
			}else {
				$("#create-account").prop('disabled', true);
			}
		}

		if(this.state.checkPassowrd != password){
			reg_err_message.innerHTML = "password not match";
			$("#create-account").prop('disabled', true);
		}else if (this.state.checkPassowrd == password){
			reg_err_message.innerHTML = "";
		}
	}

	checkPasswordOnChange(e){
		var confirmPassword = e.target.value;
		this.setState({checkPassowrd: e.target.value});
		$("#create-account").prop('disabled', true);

		if(this.state.password!=confirmPassword){
			reg_err_message.innerHTML = "password not match";
		}
		else{
			reg_err_message.innerHTML = "";
			if (checkPass.innerHTML == ""){
				$("#create-account").prop('disabled', false);
			}
			else {
				$("#create-account").prop('disabled', true);
			}
		}
	}

	componentDidMount(){
		$("#create-account").prop('disabled', true);
	}

	render(){
		return(
			<form id="myform" onSubmit={this.handelSubmit}>
			   	<span id="reg_err_message" className="err_m"/>
				
				<div className="inputs">
					<input id = "email" className = "inputbar" type="text" placeholder="Email" onChange={this.emailOnChange}/> <span id = "checkEmail" className = "message"/><br/>
					<input id="password" className = "inputbar" type="password" placeholder="Password" onChange={this.passwordOnChange}/> <span id = "checkPass" className = "message"/><br/>
					<input id="c-password" className = "inputbar" type="password" placeholder="Enter Password Again" onChange={this.checkPasswordOnChange}/> <span id = "checkConfirm" className = "message"/><br/>
				</div>
				
				<button id="create-account" className="submit_button">Create Account </button><br/>
			</form>
		) 
	}
}
