const admin = require("./mongo_login.js");
const MongoClient = require('mongodb').MongoClient

const collectionName = "weather_search_userDB";
const url = "mongodb://"+ admin.username+":"+admin.password +
		  "@ds119446.mlab.com:19446/weather_search_user";

function printArr(err, res){
	console.log(res);
}

function printAll(db,print){
	db.collection(collectionName)
	.find({},{_id:0}).toArray(print);
}

function printUser(db,print,user){
	db.collection(collectionName)
	.find(user,{_id:0}).toArray(print);
}

function mapping(){
	// for(var i=0; i<this.savedCities.length; i++){
	// 	emit(this.savedCities[i],1);
	// }
	emit("totalVisite",this.visitCount)
}

function reducing(city,cityCount){
	return Array.sum(cityCount);
}

MongoClient.connect(url,function(err,res){
	if(err){
		console.log(err);
		return;
	}

	var db = res;
	var user = {"username":"Lionel"};
	var newUser = {
				username: "Lionel",
				password: "xxxxxx",
				email: null,
				gender: "M",
				savedCities: [],
				lastVisteDate: "",
                createDate: "",
                visitCount: 0
			};

	// printUser(db,printArr,user);
	
	// db.collection(collectionName)
	// .find({username: /^w/},{_id:0}).toArray(printArr);
	
	// db.collection(collectionName)
	// .update(user,{$set:{"gender":"M"}});
	
	// db.collection(collectionName)
	// .insertOne(newUser,function(err,res){
	// 	if(err) console.log(err);
	// 	else console.log("yes");
	// })
	
	// db.collection(collectionName)
	// .deleteOne(user, function(err,res){
	// 	if(err) console.log(err);
	// 	else console.log("yes");
	// });

	// printUser(db,printArr,user);

	// db.collection(collectionName).mapReduce(mapping,reducing,{out:"map_reduce"})
	// .then(data => {
	// 	console.log("done");
	// }).catch(err => {
	// 	console.log(err);
	// });

	// db.collection("map_reduce").find({},{}).toArray(printArr);
	// db.collection("map_reduce").drop(function(err,res){
	// 	console.log(err);
	// });
	db.close();
})