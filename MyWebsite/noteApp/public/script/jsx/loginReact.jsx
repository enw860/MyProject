class UserLogIn extends React.Component{
	constructor(props){
		super(props);
		this.state = {
			email: '',
			password: '',
			isRemember: false
		};

		this.handelLogin = this.handelLogin.bind(this);
		this.handleGotoRegister = this.handleGotoRegister.bind(this);
		
		this.emailOnChange = this.emailOnChange.bind(this);
		this.passwordOnChange = this.passwordOnChange.bind(this);
		this.handleCheckbox = this.handleCheckbox.bind(this)
	}

	handelLogin(e){
		e.preventDefault();

		var url = window.location.origin + "/api/users/login";

		var userData = this.state;

	    $.ajax({
		    type:"POST",
		    contentType: 'application/json',
	        dataType: 'json',
		    url: url,
		    data: JSON.stringify(userData),
		    success: function (data) {
		    	if(data==false){
		    		$("#sig_err_message").text("Wrong Email or Password! Try again.");
				}else{
				    $("#Entry").hide();
				    showNote();
				}
			},error:function(){
	        	console.log("error");
	        }
		});
	}

	handleGotoRegister(e){
		showRegister();
	}

	emailOnChange(e){
		var email = e.target.value;
		if(email === ''){
            $("#sig_err_message").text("Email can not be empty!"); 
        } else {
        	$("#sig_err_message").text('');
        }
		this.setState({email: email});

		if(email==='') return;

		var self=this;
		var url = window.location.origin+'/api/users/getPassword/'+email;
		$.ajax({
			type: 'GET',
			url: url,
			success: function(data){
				if(data==false){
					$("#input_password").val("");
				}else{
					self.setState({
						password: data,
						isRemember: true,
					});

					$("#input_password").val(data);
					$('#remember').prop('checked', true);
				}
			}
		});
	}

	passwordOnChange(e){
		var password = e.target.value;
		if(password === ''){
            $("#sig_err_message").text("Password can not be empty!"); 
        } else {
        	$("#sig_err_message").text('');
        }
		this.setState({password: password});
	}

	handleCheckbox(e){
        let isChecked = e.target.checked;
        if(isChecked){
            this.setState({
                isRemember: true
            })
        }else{
            this.setState({
                isRemember: false
            })
        }
    }

	render(){
		return(
			<form className="myform">
			   	<span id="sig_err_message" className="err_m"/>

			   	<div className="inputs">
	 			    <input className="inputbar" type="text" placeholder="Email" id="input_email" onChange={this.emailOnChange}/><br/>
	 			    <input className="inputbar" type="password" placeholder="Password" id="input_password" onChange={this.passwordOnChange}/><br/>
 			    	<label className="rememberPassword"><input id="remember" type="checkbox" checked={this.state.isRemember} onClick={this.handleCheckbox}/>Remember Password?</label><br/>
 			    </div>
 			    <button className="submit_button" onClick={this.handelLogin}>Log In</button><br/>
			</form>
		)
	}
}
