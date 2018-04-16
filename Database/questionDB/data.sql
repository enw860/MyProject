-- Student, Class, Quiz, Question_Bank, Multiple_Choice, MC_OP, Numeric_question, Num_Hint, TF_question, InClass, In_quize, Quiz_For_Class, Response

INSERT INTO Student values
('0998801234', 'Lena', 'Headey'), 
('0010784522', 'Peter', 'Dinklage'), 
('0997733991', 'Emilia', 'Clarke'), 
('5555555555', 'Kit', 'Harrington'), 
('1111111111', 'Sophie', 'Turner'), 
('2222222222', 'Maisie', 'Williams');

INSERT INTO Class values(366, 'Miss Nyers'),(120, 'Mr Higgins');

INSERT INTO Quiz values('Pr1-220310', 'Citizenship Test Practise Questions', '01/10/2018 01:30 PM', TRUE);

INSERT INTO Question_Bank values
(782, 'Multiple-choice'), 
(566, 'True-False'), 
(601, 'Numeric'), 
(625, 'Multiple-choice'), 
(790, 'Multiple-choice');

INSERT INTO MC_OP values
(782, 1, 'To pledge your loyalty to the Sovereign, Queen Elizabeth II', null,TRUE), 
(782, 2, 'To pledge your allegiance to the flag and fulfill the duties of a Canadian', null,FALSE), 
(782, 3, 'To pledge your allegiance to the flag and fulfill the duties of a Canadian', 'Think regally',FALSE), 
(782, 4, 'To pledge your loyalty to Canada from sea to sea', null,FALSE), 
(625, 1, 'The first railway to cross Canada', 'The Underground Railroad was generally south to north, not east-west.',FALSE), 
(625, 2, 'The CPR''s secret railway line', 'The Underground Railroad was secret, but it had nothing to do with trains.',FALSE), 
(625, 3, 'The TTC subway system', 'The TTC is relatively recent; the Underground Railroad was in operation over 100 years ago.',FALSE), 
(625, 4, 'A network used by slaves who escaped the United States into Canada', null,TRUE), 
(790, 1, 'They attacked American merchant ships', null,FALSE),
(790, 2, 'They expanded their defence system, including Fort York', null,FALSE),
(790, 3, 'They burned down the White House in Washington D.C.', null,TRUE),
(790, 4, 'They captured Niagara Falls', null,FALSE);

INSERT INTO Multiple_Choice values
(782, 'What do you promise when you take the oath of citizenship?', 1), 
(625, 'What is the Underground Railroad?', 4), 
(790, 'During the War of 1812 the Americans burned down the Parliament Buildings inYork (now Toronto). What did the British and Canadians do in return?', 3);

INSERT INTO Numeric_question values
(601, 'During the "Quiet Revolution," Quebec experienced rapid change. In what decade did this occur? (Enter the year that began the decade, e.g., 1840.)', 1960);

INSERT INTO Num_Hint values
(601, 1, 1800, 1900, 'The Quiet Revolution happened during the 20th Century.'), 
(601, 2, 2000, 2010, 'The Quiet Revolution happened some time ago.'), 
(601, 3, 2020, 3000, 'The Quiet Revolution has already happened!');

INSERT INTO TF_question values(566, 'The Prime Minister, Justin Trudeau, is Canada''s Head of State.', FALSE);

INSERT INTO In_Class values
('0998801234', 120, 8), 
('0010784522', 120, 8), 
('0997733991', 120, 8), 
('5555555555', 120, 8), 
('1111111111', 120, 8), 
('2222222222', 366, 5);

INSERT INTO In_quiz values
('Pr1-220310', 601, 2), 
('Pr1-220310', 566, 1), 
('Pr1-220310', 790, 3), 
('Pr1-220310', 625, 2);

INSERT INTO Quiz_For_Class values('Pr1-220310', 8, 120);

INSERT INTO Response values
('0998801234', 'Pr1-220310', 601, null, 1950, null), 
('0998801234', 'Pr1-220310', 566, null, null, FALSE), 
('0998801234', 'Pr1-220310', 790, 2, null, null),
('0998801234', 'Pr1-220310', 625, 4, null, null),
('0010784522', 'Pr1-220310', 601, null, 1960, null), 
('0010784522', 'Pr1-220310', 566, null, null, FALSE), 
('0010784522', 'Pr1-220310', 790, 3, null, null),
('0010784522', 'Pr1-220310', 625, 4, null, null),
('0997733991', 'Pr1-220310', 601, null, 1960, null), 
('0997733991', 'Pr1-220310', 566, null, null, TRUE), 
('0997733991', 'Pr1-220310', 790, 3, null, null),
('0997733991', 'Pr1-220310', 625, 2, null, null),
('5555555555', 'Pr1-220310', 566, null, null, FALSE), 
('5555555555', 'Pr1-220310', 790, 4, null, null);
