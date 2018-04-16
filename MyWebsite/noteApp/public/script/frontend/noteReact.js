class Note extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			id: -1,
			title: "",
			content: "",
			searchText: ""
		};

		this.noteSwitch = this.noteSwitch.bind(this);

		this.titleOnChange = this.titleOnChange.bind(this);
		this.contentOnChange = this.contentOnChange.bind(this);
		this.searchTextHandler = this.searchTextHandler.bind(this);
		this.endOfSearchHandler = this.endOfSearchHandler.bind(this);

		this.createNote = this.createNote.bind(this);
		this.deleteNote = this.deleteNote.bind(this);
		this.saveHandler = this.saveHandler.bind(this);
		this.intervalSaving = this.intervalSaving.bind(this);

		this.highlightText = this.highlightText.bind(this);
	}

	noteSwitch(note) {
		$("#Note_Title").prop("disabled", false);
		$("#Note_Content").prop("disabled", false);

		if (this.state.id !== -1) {
			this.saveHandler();
		}

		this.setState({
			id: note.id,
			title: note.title,
			content: note.content
		});

		$("#Note_Title").val(note.title);
		$("#Note_Content").val(note.content);

		if (note.id >= 0) {
			$("#save_button").prop('disabled', false);
		} else {
			$("#save_button").prop('disabled', true);
		}

		this.highlightText();
	}

	titleOnChange(e) {
		var title = e.target.value;
		this.setState({
			title: title
		});

		if (this.state.id < 0) {
			if (title !== "" || this.state.content !== "") {
				$("#save_button").prop('disabled', false);
			} else if (title == "" || this.state.content == "") {
				$("#save_button").prop('disabled', true);
			}
		}
	}

	contentOnChange(e) {
		var content = e.target.value;
		this.setState({
			content: content
		});

		if (this.state.id < 0) {
			if (this.state.title !== "" || content !== "") {
				$("#save_button").prop('disabled', false);
			} else if (this.state.title == "" || content == "") {
				$("#save_button").prop('disabled', true);
			}
		}
	}

	searchTextHandler(e) {
		var searchText = e.target.value;
		this.setState({
			searchText: searchText
		});

		this.noteSwitch({
			id: -1,
			title: "",
			content: "",
			searchText: ""
		});
		if (searchText === "") {
			$("#Note_Title").prop("disabled", false);
			$("#Note_Content").prop("disabled", false);
		} else {
			$("#Note_Title").prop("disabled", true);
			$("#Note_Content").prop("disabled", true);
		}

		showNote();
		this.highlightText();
	}

	endOfSearchHandler() {
		this.setState({
			searchText: ""
		});

		this.noteSwitch({
			id: -1,
			title: "",
			content: "",
			searchText: ""
		});

		$("#searchInput").val("");
		$("#Note_Title").prop("disabled", false);
		$("#Note_Content").prop("disabled", false);

		showNote();
	}

	highlightText() {
		var self = this;
		$(function () {
			$('#Note_Content').highlightWithinTextarea({
				highlight: self.state.searchText
			});
		});

		$(function () {
			$('#Note_Title').highlightWithinTextarea({
				highlight: self.state.searchText
			});
		});
	}

	deleteNote(id) {
		this.noteSwitch({
			id: -1,
			title: "",
			content: "",
			searchText: ""
		});

		var self = this;
		var url = window.location.origin + "/api/users/deleteNote/" + id;

		$.ajax({
			type: "DELETE",
			cache: false,
			url: url,
			success: function (data) {
				if (data === false) {
					console.log("unable to delete this note");
				} else {
					showNote();
				}
			}, error: function (XMLHttpRequest, textStatus, errorThrown) {
				console.log("Network Error: unable to delete this note");
			}
		});
	}

	createNote(title, content) {
		var self = this;
		var emptyNote = {
			title: title,
			content: content
		};

		var url = window.location.origin + "/api/users/addNote";

		$.ajax({
			type: "POST",
			contentType: 'application/json',
			dataType: 'json',
			url: url,
			data: JSON.stringify({ newNote: emptyNote }),
			success: function (data) {
				if (data === false) {
					console.log("unable to create Note");
				} else {
					showNote();

					//case that user start write note with out create a note
					if (self.state.id === -1) {
						self.setState({ id: data.id });
					} else {
						self.noteSwitch(data);
					}
				}
			}, error: function (XMLHttpRequest, textStatus, errorThrown) {
				console.log("Network Error: unable to create note");
			}
		});
	}

	saveHandler() {
		if (this.state.id < 0) {
			if (this.state.title !== "" || this.state.content !== "") {
				this.createNote(this.state.title, this.state.content);
			}
			return;
		}

		var url = window.location.origin + "/api/users/noteSave";

		$.ajax({
			type: "PUT",
			contentType: 'application/json',
			dataType: 'json',
			url: url,
			data: JSON.stringify({ note: this.state }),
			success: function (data) {
				if (data === false) {
					console.log("unable to save");
				} else {
					showNote();
				}
			}, error: function (XMLHttpRequest, textStatus, errorThrown) {
				console.log("Network Error: unable to save note");
			}
		});
	}

	intervalSaving() {
		var seconds = 5;
		window.setInterval("clickSave();", seconds * 1000);
	}

	componentDidMount() {
		this.intervalSaving();
		$("#save_button").prop('disabled', true);
	}

	render() {
		var userData = this.props.userData;
		var originNotes = userData.notes;
		var notes = [];
		var substring = this.state.searchText;
		var isNotSearching = substring === "";

		if (isNotSearching) {
			notes = originNotes;
		} else {
			//filter search result
			for (var i = 0; i < originNotes.length; i++) {
				var isInTitle = originNotes[i].title.indexOf(substring);
				var isInContent = originNotes[i].content.indexOf(substring);
				if (isInContent >= 0 || isInTitle >= 0) {
					notes.push(originNotes[i]);
				}
			}
		}

		var coverContext = function (str, limit) {
			if (str.length > 10) {
				var temp = str.substring(0, limit);
				temp = temp + "...";
				return temp;
			}
			return str;
		};

		return React.createElement(
			"div",
			{ id: "NoteBody" },
			React.createElement(
				"div",
				{ id: "result" },
				React.createElement(
					"div",
					{ id: "search" },
					React.createElement("input", { type: "text", placeholder: "Searching text", id: "searchInput", onChange: this.searchTextHandler })
				),
				React.createElement(
					"div",
					{ id: "NoteBlock" },
					notes.map(note => React.createElement(
						"div",
						{ className: "briefNote", key: "note_" + note.id },
						React.createElement(
							"div",
							{ className: "noteTag", onClick: () => this.noteSwitch(note) },
							React.createElement(
								"span",
								null,
								"Title: ",
								coverContext(note.title, 13)
							),
							React.createElement("br", null),
							React.createElement(
								"span",
								null,
								"Content: ",
								coverContext(note.content, 10)
							)
						),
						React.createElement(
							"div",
							{ onClick: () => this.deleteNote(note.id), className: "idClose" },
							"X"
						)
					)),
					isNotSearching ? React.createElement(
						"div",
						{ className: "briefNote", onClick: () => this.createNote("", "") },
						React.createElement(
							"a",
							{ id: "newNote" },
							"+ NEW NOTE"
						)
					) : React.createElement(
						"div",
						{ className: "briefNote", onClick: this.endOfSearchHandler },
						React.createElement(
							"a",
							null,
							"End of search"
						)
					)
				)
			),
			React.createElement(
				"div",
				{ id: "NoteContent" },
				React.createElement("input", { type: "text", placeholder: "Title", id: "Note_Title", onChange: this.titleOnChange }),
				React.createElement("br", null),
				React.createElement(
					"div",
					{ id: "contentwrapper" },
					React.createElement("textarea", { placeholder: "Context", id: "Note_Content", rows: "25", onChange: this.contentOnChange }),
					React.createElement("br", null)
				),
				React.createElement(
					"button",
					{ id: "save_button", onClick: this.saveHandler },
					"Save"
				),
				React.createElement("br", null)
			)
		);
	}
}

