drop schema if exists quizschema cascade;
create schema quizschema;
set search_path to quizschema;


-------------------------------Type Define----------------------------------------
CREATE TYPE Question_Type AS ENUM('Multiple-choice', 'Numeric','True-False');
-------------------------------------END------------------------------------------

-------------------------------Check Functions------------------------------------
CREATE FUNCTION check_In_Range(qid INT, low INT, up INT) RETURNS BOOLEAN AS $$
DECLARE
    ret integer;
BEGIN
	SELECT num_answer INTO ret
	FROM Numeric_question
	WHERE question_id = qid;
	
	IF (ret>up OR ret<low) THEN RETURN true;
	ELSE RETURN false;
	END IF;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION check_answer_in_option(qid INT, op_id INT) RETURNS BOOLEAN AS $$
DECLARE
	ret integer;
BEGIN
	SELECT count(*) INTO ret FROM MC_OP
	WHERE question_id=qid AND mc_op_id=op_id;

	IF (ret=1) THEN RETURN true;
	ELSE RETURN false;
	END IF;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION check_at_least_2_option(qid INT) RETURNS BOOLEAN AS $$
DECLARE
	ret integer;
BEGIN
	SELECT count(*) INTO ret FROM MC_OP WHERE question_id=qid;

	IF (ret>1) THEN RETURN true;
	ELSE RETURN false;
	END IF;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION check_mc_op_hint(answer BOOLEAN,hint VARCHAR(200)) RETURNS BOOLEAN AS $$
BEGIN
	IF (answer AND hint IS NULL) THEN RETURN true;
	ELSEIF (NOT answer) THEN RETURN true;
	ELSE RETURN false;
	END IF;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION check_response(ans1 INT, ans2 INT, ans3 BOOLEAN) RETURNS BOOLEAN AS $$
BEGIN
	IF (ans1 IS NOT NULL AND ans2 IS NULL AND ans3 IS NULL) THEN RETURN true;
	ELSEIF (ans1 IS NULL AND ans2 IS NOT NULL AND ans3 IS NULL) THEN RETURN true;
	ELSEIF (ans1 IS NULL AND ans2 IS NULL AND ans3 IS NOT NULL) THEN RETURN true;
	ELSE RETURN false;
	END IF;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION check_qualified_student(sid_in VARCHAR(10), quiz_id_in VARCHAR(50)) RETURNS BOOLEAN AS $$
DECLARE
	ret integer;
BEGIN
	SELECT count(*) INTO ret
	FROM In_Class NATURAL JOIN Quiz_For_Class
	WHERE sid=sid_in AND quiz_id=quiz_id_in;

	IF (ret=1) THEN RETURN true;
	ELSE RETURN false;
	END IF;
END;
$$ LANGUAGE plpgsql;
-------------------------------End Functions--------------------------------------


------------------------------------Sechema---------------------------------------
CREATE TABLE Student(
	sid VARCHAR(10) PRIMARY KEY,
	first_name VARCHAR(50) NOT NULL,
	last_name VARCHAR(50) NOT NULL,
	CHECK (sid SIMILAR TO '[0-9]{10}')
);

CREATE TABLE Class(
	room VARCHAR(50) PRIMARY KEY,
	teacher VARCHAR(50)
);

CREATE TABLE Quiz(
	quiz_id VARCHAR(50) PRIMARY KEY,
	title VARCHAR(100),
	due_date TIMESTAMP,
	show_hint BOOLEAN DEFAULT TRUE
);

CREATE TABLE Question_Bank(
	question_id INT PRIMARY KEY,
	type Question_Type NOT NULL
);

CREATE TABLE MC_OP(
	question_id INT REFERENCES Question_Bank(question_id)
		 ON DELETE CASCADE, 
	mc_op_id INT, 
	option VARCHAR(200) NOT NULL,
	hint VARCHAR(200), 
	is_Answer BOOLEAN,
	PRIMARY KEY(question_id, mc_op_id),
	CHECK(check_mc_op_hint(is_Answer,hint))
);

CREATE TABLE Multiple_Choice(
	question_id INT PRIMARY KEY REFERENCES Question_Bank(question_id) 
		ON DELETE CASCADE,
	text VARCHAR(500) NOT NULL,
	mc_answer INT NOT NULL,
	CHECK(check_answer_in_option(question_id, mc_answer)),
	CHECK(check_at_least_2_option(question_id))
);

CREATE TABLE Numeric_question(
	question_id INT PRIMARY KEY REFERENCES Question_Bank(question_id)
		 ON DELETE CASCADE,
	text VARCHAR(500) NOT NULL,
	num_answer INT NOT NULL
);

CREATE TABLE Num_Hint(
	question_id INT REFERENCES Question_Bank(question_id)
		 ON DELETE CASCADE,
	num_hint_id INT ,
	lower_bound INT	NOT NULL,
	upper_bound INT NOT NULL, 
	hint VARCHAR(200), 
	PRIMARY KEY (question_id, num_hint_id),
	CHECK(check_In_Range(question_id,lower_bound,upper_bound))
);

CREATE TABLE TF_question(
	question_id INT REFERENCES Question_Bank(question_id)
		 ON DELETE CASCADE,
	text VARCHAR(500) NOT NULL,
	TF_answer BOOLEAN NOT NULL,
	UNIQUE(question_id)
);

CREATE TABLE In_Class(
	sid VARCHAR(10) REFERENCES Student(sid)
		 ON DELETE CASCADE,
	room VARCHAR(50) REFERENCES Class(room)
		 ON DELETE CASCADE,
	grade VARCHAR(50) NOT NULL,
	PRIMARY KEY(sid, room, grade)
);

CREATE TABLE In_quiz(
	quiz_id VARCHAR(50) REFERENCES Quiz(quiz_id)
		ON DELETE CASCADE,
	question_id INT REFERENCES Question_Bank(question_id)
		ON DELETE CASCADE,
	weight INT NOT NULL,
	PRIMARY KEY(quiz_id, question_id, weight)
);

CREATE TABLE Quiz_For_Class(
	quiz_id VARCHAR(50) REFERENCES Quiz(quiz_id),
	grade VARCHAR(50) NOT NULL,
	room VARCHAR(50) REFERENCES Class(room),
	PRIMARY KEY(quiz_id, room, grade)
);

CREATE TABLE Response(
	sid VARCHAR(10) REFERENCES Student(sid)
		ON DELETE CASCADE,
	quiz_id VARCHAR(50) REFERENCES Quiz(quiz_id)
		ON DELETE CASCADE,
	question_id INT REFERENCES Question_Bank(question_id)
		 ON DELETE CASCADE,
	mc_answer INT,
	num_answer INT,
	TF_answer BOOLEAN,
	PRIMARY KEY(sid, quiz_id, question_id),
	CHECK(check_response(mc_answer,num_answer,TF_answer)),
	CHECK(check_qualified_student(sid,quiz_id))
);
