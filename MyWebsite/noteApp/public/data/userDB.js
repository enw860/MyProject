const admin = require("./mongo_login.js");
const MongoClient = require('mongodb').MongoClient

const collectionName = "NoteTaker";
const url = "mongodb://"+ admin.username+":"+admin.password +
		  "@ds217349.mlab.com:17349/note_taker";

var userDB = (function () {
	//user to hash password
	function hash(str) {
        var hash = 5381;
        var i = str.length;
        while(i) {
            hash = (hash * 33) ^ str.charCodeAt(--i);
        }
        return hash >>> 0;
    }

    //compare if input password matches setPassword 
    function hashCompare(password, setPassword){
        password = hash(password);
        return (password===setPassword);
    }

    //return a promise that contains user's info
    function getUser(db, email){
    	return db.collection(collectionName)
    		.find({email:email},{_id:false})
			.toArray();
    }

    //return a promise that is a boolean value
    //indicates if a username in current DB
    function isEmailNotExist(userList){
    	return userList.then(data => {
    		var size = data.length;
    		return (size==0);
    	});
    }

    //do not return user's password
    function encapUserInfo(user){
    	var encapData = user;
    	delete encapData.password;
        return encapData;
    }

   return {
		createUser: function(user){
    		var newUser = {
    			email: user.email, 
    			password: hash(user.password),
    			notes: []
    		}

    		return new Promise(function(resolve, reject){
    			MongoClient.connect(url, function(err,res){
					var db = res;
					var userList = getUser(db, newUser.email);
					var emailNotExist = isEmailNotExist(userList);
					
					emailNotExist.then(data=>{
						if(!data){
							db.close();
							resolve(false);
						}else{
							db.collection(collectionName)
			    			.insertOne(newUser, function(err, res){
			    				if(err) resolve(false);
			    			})
			    			db.close();
							resolve(true);
						}
					})
				});
    		})
    	}, logIn: function(email, password){
			return new Promise(function (resolve, reject){
				MongoClient.connect(url, function(err,res){
					var db = res;
					var userList = getUser(db, email);
					var emailNotExist = isEmailNotExist(userList);
					
					emailNotExist.then(data=>{
						if(data){
							db.close();
							resolve(false);
						}else{
							db.close();
							userList.then(user=>{
								var isPassword = hashCompare(password, user[0].password);
								if(isPassword){
									var encapData = encapUserInfo(user[0])
									resolve(encapData);	
								}else{
									resolve(false);
								}
							})
						}
					})
				})
			})
		}, updateNote: function(email, newNote){
			var updateUser = {email:email};
			return new Promise(function (resolve, reject){
				MongoClient.connect(url, function(err,res){
					var db = res;
					var userList = getUser(db, email);
					var emailNotExist = isEmailNotExist(userList);
					
					emailNotExist.then(data=>{
						if(data){
							db.close();
							resolve(false);
						}else{
							userList.then(users=>{
								var user = users[0]

								user.notes = newNote
								db.collection(collectionName)
								.updateOne(updateUser, user, function(err, res) {
								    if (err) {
								    	db.close();
								    	resolve(false);
								    }
								});
								db.close();
								resolve(true);	
							})
						}
					})
				})
			})
		}
    }
})();

module.exports = userDB;