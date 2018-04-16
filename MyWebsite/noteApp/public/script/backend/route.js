const request = require('request');
const users = require('../../data/userDB.js');

const sessionExpireTime = 15 * 60 * 1000;

function route(app) {
  	app.get('/', (req, res) => {
    	res.redirect('/noteTaker');
  	});

  	app.get('/noteTaker', (req, res) => {
    	res.render('noteTaker');
  	});

  	app.post('/api/users/login', (req, res) => {
    	const user = {
      		email: req.body.email,
      		password: req.body.password,
      		remember: req.body.isRemember
    	};

    	users.logIn(user.email, user.password).then((data) => {
      		if (data !== false) {
	        	//once users signed in, sign them a 15 mins session
	        	res.cookie('current_user', user.email, { maxAge: sessionExpireTime, httpOnly: true });

		        //if user choose remember their account, then save a 
		        //signed non-expire cookie on user's browser
		        //if not, then remove already exists non-expire cookie
		        //for a user
		        if (user.remember) {
		          res.cookie(user.email, user, { httpOnly: true, signed: true });
		        } else {
		          res.clearCookie(user.email);
		        }

		        //change user state, and save their data in their session
		        //those data will gone once user loged out or 
		        //session expired
		        req.session.authenticated = true;
		        req.session.user = data;
		        req.session.cookie.maxAge = sessionExpireTime;

	        	res.send(data);
	      	} else res.send(false);
	    });
	});
	
	//get password that user store in cookie
	app.get('/api/users/getPassword/:email', (req, res) => {
	    const email = req.params.email;
	    const user = req.signedCookies[email];
	    if (user == null) {
	      	res.send(false);
	    } else {
	      	res.send(user.password);
	    }
	});

	//get logged in user's date
  	app.get('/api/users/data', (req, res) => {
    	const user = req.session.user;
    	if (user === undefined) {
    	  res.send(false);
    	} else if (req.session.authenticated) {
    	  res.send(user);
    	} else {
    	  res.send(false);
    	}
  	});

  	//create new user
	app.post('/api/users', (req, res) => {
	    const newCount = {
	      	email: req.body.email,
	      	password: req.body.password,
	      	notes: []
	    };

	    users.createUser(newCount).then((data) => {
	      	res.send(data);
	    });
  	});

	//create a new note
  	app.post('/api/users/addNote', (req, res) => {
	    const user = req.session.user;
	    if (req.session.authenticated) {
	      	var newNote = req.body.newNote;
	      	const notes = user.notes;
	      	newNote.id = 0;
	      	
	      	for(let i=0; i<notes.length; i++) {
	      		if(notes[i].id >= newNote.id){
	      			newNote.id = notes[i].id + 1;
	      		}
	      	}

	      	notes.push(newNote);
	      	req.session.user.notes = notes;
	      	
	      	users.updateNote(user.email, notes).then((data) => {
	        	if(data) res.send(newNote);
	      	});
	    } else {
	      	res.send(false);
	    }
  	});

  	//update a note
  	app.put('/api/users/noteSave', (req, res) => {
	    const user = req.session.user;
	    if (req.session.authenticated) {
	      	var saveNote = req.body.note;
	      	const notes = user.notes;
	      	
	      	for(let i=0; i<notes.length; i++) {
	      		if(notes[i].id === saveNote.id){
	      			notes[i] = saveNote;
	      		}
	      	}

	      	req.session.user.notes = notes;
	      	
	      	users.updateNote(user.email, notes).then((data) => {
	        	res.send(data);
	      	});
	    } else {
	    	res.send(false);
	    } 
  	});

  	//delete a note
	app.delete('/api/users/deleteNote/:id', (req, res) => {
    	const user = req.session.user;
    	if (req.session.authenticated) {
	      	var id = req.params.id;
	      	id = parseInt(id);
	      	const notes = user.notes;

	      	for(let index=0; index<notes.length; index++){
	      		if(notes[index].id === id){
	      			notes.splice(index, 1);
	      			users.updateNote(user.email, notes).then((data) => {
				        res.send(data);
			       	});
			       	return;
	      		}
	      	}
    	} 

    	res.send(false);
  	});

	app.get('/users/logout', (req, res) => {
	    //remove all user's data
	    res.clearCookie('current_user');
	    req.session.authenticated = false;
	    req.session.cookie.expires = new Date(Date.now());
	    req.session.cookie.maxAge = 0;

	    res.redirect('/');
  	});

	//allow client check if session is expired
  	app.get('/api/users/session_expired', (req, res) => {
    	const isExpired = (req.session.cookie.expires == null);
    	res.send(isExpired);
  	});
}

module.exports = route;



