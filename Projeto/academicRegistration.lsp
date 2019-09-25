;Academic Registration
;Pontifícia Universidade Católica
;Cesar Marrote Manzano
;Marcos Antonio Vasconcellos Junior 


;********************************************************************
;=========================== TOOLS ============================

;REMOVE ONE LIST FROM ANOTHER
(DEFUN REMOVE-LIST-LIST (LISTA_ORIG LISTA_AUX)
	(IF(NULL LISTA_AUX)
		LISTA_ORIG
		(REMOVE-LIST-LIST (REMOVE (CAR LISTA_AUX) LISTA_ORIG) (CDR LISTA_AUX))))
;<>


;REMOVE LIST REPETITIONS
(DEFUN REMOVEDUPLICADOS (LISTA)
	(COND	((NULL LISTA) LISTA)
			((MEMBER (CAR LISTA) (CDR LISTA))(REMOVEDUPLICADOS (CDR LISTA)))
			(T (CONS (CAR LISTA) (REMOVEDUPLICADOS (CDR LISTA))))))
;<>


;MAKE SURE ELEMENT "ELEMENTO" BELONGS TO "LISTA" LIST
(DEFUN MEMBRO? (ELEMENTO LISTA)
	(IF(NULL (CAR LISTA))
		NIL
		(IF (EQL ELEMENTO (CAR LISTA))
		T
		(MEMBRO? ELEMENTO (CDR LISTA)))))
;<>


;MAKE SURE THE DISCIPLINE HAS ANY STUDENT OR TEACHER 
;IF NOT REMOVE THIS DISCIPLINE FROM DATABASE
(DEFUN VERIFICA-DISCIPLINA (BD)
	(IF (NULL BD)
		NIL
		(IF (AND
				(NULL (CDAAR BD))
				(NULL (CDR(CDAAAR BD)))
				(NULL (CDR(CAAAAR BD)))
			)
			(VERIFICA-DISCIPLINA (CDR BD))
			(CONS (CAR BD) (VERIFICA-DISCIPLINA (CDR BD))))))
;<>

;********************************************************************



;********************************************************************
;====================== SUBJECTS FUNCTIONS ====================

;LIST ALL SUBJECTS
(DEFUN DISCIPLINAS? (BD)
	(IF (NULL BD)
		NIL
		(CONS (CDAR BD) (DISCIPLINAS? (CDR BD)))))
;<>



;REGISTERED STUDENTS -> SUBJECT AND CLASS
(DEFUN MATRICULADOS? (DISCIPLINA TURMA BD)
	(IF(AND(NULL (CDR BD))(NULL (CAR BD)))
		NIL
		(IF(EQL DISCIPLINA (CDAR BD))
			(IF(EQL TURMA 'T2)
				(CDDR(CAAAR BD))
				(IF(EQL TURMA 'T1)
					(CDR(CAAAAR BD))
					(MATRICULADOS? DISCIPLINA TURMA (CDR BD)))))))
;<>


;REGISTERED TEACHERS -> SUBJECT
(DEFUN VINCULADOS? (DISCIPLINA BD)
	(IF (AND
			(NULL (CDR BD))
			(NULL (CAR BD))
		)
		NIL
		(IF(EQUAL DISCIPLINA (CDAR BD))
			(CDAAR BD)
			(VINCULADOS? DISCIPLINA (CDR BD)))))
;<>

;********************************************************************



;********************************************************************
;======================== STUDENTS FUNCTIONS =======================

;LIST ALL STUDENTS
(DEFUN BUSCAALUNOS (BD)
	(IF	(AND
			(NULL (CDR BD))
			(OR(NULL (CDR (CAAAAR BD)))(NULL (CDDR(CAAAR BD))))
		)
		NIL
		(APPEND
			(APPEND (CDAR(CAAAR BD)) (CDDR(CAAAR BD)))
			(ALUNOS? (CDR BD)))))
(DEFUN ALUNOS? (BD)(REMOVEDUPLICADOS (BUSCAALUNOS BD)))
;<>


;ALL SUBJECTS OF A STUDENT
(DEFUN CURSA? (ALUNO BD)
	(IF (AND(NULL (CDR BD))(NULL (CAR BD)))
		NIL
		(IF (OR
				(MEMBRO? ALUNO (CDR(CDAAAR BD)))
				(MEMBRO? ALUNO (CDR(CAAAAR BD)))
			)
			(CONS (CDAR BD) (CURSA? ALUNO (CDR BD)))
			(CURSA? ALUNO (CDR BD)))))
;<>

;********************************************************************



;********************************************************************
;======================= TEACHERS FUNCTIONS ===================

;LIST ALL TEACHERS
(DEFUN BUSCAPROFESSORES (BD)
	(IF(AND(NULL (CDR BD))(NULL (CAR BD)))
		NIL
		(APPEND (CDAAR BD)(PROFESSORES? (CDR BD)))))
(DEFUN PROFESSORES? (BD)(REMOVEDUPLICADOS (BUSCAPROFESSORES BD)))
;<>


;ALL SUBJECTS OF A TEACHER
(DEFUN MINISTRA? (PROFESSOR BD)
	(IF (AND(NULL (CDR BD))(NULL (CAR BD)))
		NIL
		(IF (MEMBER PROFESSOR (CDAAR BD))
			(CONS (CDAR BD) (MINISTRA? PROFESSOR (CDR BD)))
			(MINISTRA? PROFESSOR (CDR BD)))))
;<>

;********************************************************************



;********************************************************************
;========================  REGISTRATION FUNCTIONS (TEACHERS)  ====================

;REGISTER TEACHERS IN SELECTED SUBJECTS
(DEFUN VINCULAR (PROFESSORES DISCIPLINAS BD)
	(IF	(NULL DISCIPLINAS)
		BD
		(IF	(NULL BD)
			(CONS
				(CONS
					(CONS
						(CAAAR BD)
						(REMOVEDUPLICADOS (APPEND PROFESSORES (CDAAR BD)))
					)
					(CAR DISCIPLINAS)
				)
				(VINCULAR PROFESSORES (REMOVE (CAR DISCIPLINAS) DISCIPLINAS) (CDR BD))
			)
			
			(IF (MEMBRO? (CDAR BD) DISCIPLINAS)
				(CONS 
					(CONS
						(CONS
							(CAAAR BD)
							(REMOVEDUPLICADOS (APPEND PROFESSORES (CDAAR BD)))
						)
						(CDAR BD)
					) 
					(VINCULAR PROFESSORES (REMOVE (CDAR BD) DISCIPLINAS) (CDR BD))
				)
			
				(CONS (CAR BD) (VINCULAR PROFESSORES DISCIPLINAS (CDR BD)))
			)
		)
	)
)


;<>



;REMOVE TEACHER FROM A SUBJECT
(DEFUN DESVINCULAR-PROF (PROFESSORES DISCIPLINAS BD)
	(IF (NULL BD)
		NIL
		(IF (NULL (CDAAR BD))
			(CONS (CAR BD) (DESVINCULAR-PROF PROFESSORES DISCIPLINAS (CDR BD)))
			(IF (MEMBRO? (CDAR BD) DISCIPLINAS)
				(CONS
					(CONS
						(CONS
							(CAAAR BD)
							(REMOVE-LIST-LIST (CDAAR BD) PROFESSORES)
						)
						(CDAR BD)
					)
					(DESVINCULAR-PROF PROFESSORES DISCIPLINAS (CDR BD))
				)
				(CONS (CAR BD) (DESVINCULAR-PROF PROFESSORES DISCIPLINAS (CDR BD)))))))
;<>


;REMOVE TEACHER FROM A SUBJECT
;CALLS "VERIFICA-DISCIPLINA" TO SEE IF THE SUBJECT DOESN'T NEED TO BE REMOVED
(DEFUN REMOVER-VINCULO (PROFESSORES DISCIPLINAS BD)
	(VERIFICA-DISCIPLINA (DESVINCULAR-PROF PROFESSORES DISCIPLINAS BD)))
;<>

;********************************************************************



;********************************************************************
;======================== REGISTRATION FUNCTIONS (STUDENTS)====================

;REGISTER STUDENTS IN SELECTED SUBJECTS
(DEFUN MATRICULAR (ALUNOS DISCIPLINAS TURMA BD)
	(IF (NULL DISCIPLINAS)
		BD
		(IF (NULL BD)
			(IF (EQL TURMA 'T1)
				(CONS
					(CONS
						(CONS
							(CONS
								(CONS
									'T1   
									(REMOVEDUPLICADOS (APPEND ALUNOS (CDAR(CAAAR BD))))
								)
								(CONS
									'T2
									(CDDR(CAAAR BD))
								)
							)
							(CDAAR BD)
						)
						(CAR DISCIPLINAS)
					)
					(MATRICULAR ALUNOS (REMOVE (CAR DISCIPLINAS) DISCIPLINAS) TURMA (CDR BD))
				)
				
				(CONS
					(CONS
						(CONS
							(CONS
								(CONS
									'T1   
									(CDR(CAAAAR BD))
								)
								(CONS
									'T2
									(REMOVEDUPLICADOS (APPEND ALUNOS (CDDR(CAAAR BD))))
								)
							)
							(CDAAR BD)
						)
						(CAR DISCIPLINAS)
					)
					(MATRICULAR ALUNOS (REMOVE (CAR DISCIPLINAS) DISCIPLINAS) TURMA (CDR BD))
				)
				
			)
			
			(IF (MEMBRO? (CDAR BD) DISCIPLINAS)
				(IF (EQL TURMA 'T1)
					(CONS
						(CONS
							(CONS
								(CONS
									(CONS
										'T1   
										(REMOVEDUPLICADOS (APPEND ALUNOS (CDAR(CAAAR BD))))
									)
									(CONS
										'T2
										(CDDR(CAAAR BD))
									)
								)
								(CDAAR BD)
							)
							(CDAR BD)
						)
						(MATRICULAR ALUNOS (REMOVE (CDAR BD) DISCIPLINAS) TURMA (CDR BD))
					)
					
					(CONS
						(CONS
							(CONS
								(CONS
									(CONS
										'T1  
										(CDR(CAAAAR BD))
									)
									(CONS
										'T2
										(REMOVEDUPLICADOS (APPEND ALUNOS (CDDR(CAAAR BD))))
									)
								)
								(CDAAR BD)
							)
							(CDAR BD)
						)
						(MATRICULAR ALUNOS (REMOVE (CDAR BD) DISCIPLINAS) TURMA (CDR BD))
					)
					
				)
				(CONS (CAR BD) (MATRICULAR ALUNOS DISCIPLINAS TURMA (CDR BD)))
			)
		)	
	)
)
;<>




;REMOVE STUDENT FROM A SUBJECT
(DEFUN DESVINCULAR-ALUNO (ALUNOS DISCIPLINAS TURMA BD)
	(IF	(NULL BD)
		NIL
		(IF	(EQL TURMA 'T1)
			(IF	(NULL (CDR(CAAAAR BD)))
				(CONS (CAR BD) (DESVINCULAR-ALUNO ALUNOS DISCIPLINAS TURMA (CDR BD)))
				(IF	(MEMBRO? (CDAR BD) DISCIPLINAS)
					(CONS
						(CONS
							(CONS
								(CONS
									(CONS
										'T1
										(REMOVE-LIST-LIST (CDR(CAAAAR BD)) ALUNOS)
									)
									(CONS
										'T2
										(CDDR(CAAAR BD))
									)
								)
								(CDAAR BD)
							)
							(CDAR BD)
						)
						(DESVINCULAR-ALUNO ALUNOS DISCIPLINAS TURMA (CDR BD))
					)
					(CONS (CAR BD) (DESVINCULAR-ALUNO ALUNOS DISCIPLINAS TURMA (CDR BD)))
				)
			)
			(IF	(EQL TURMA 'T2)
				(IF (NULL (CDDR(CAAAR BD)))
					(CONS (CAR BD) (DESVINCULAR-ALUNO ALUNOS DISCIPLINAS TURMA (CDR BD)))
					(IF	(MEMBRO? (CDAR BD) DISCIPLINAS)
						(CONS
							(CONS
								(CONS
									(CONS
										(CONS
											'T1
											(CDR(CAAAAR BD))
										)
										(CONS
											'T2
											(REMOVE-LIST-LIST (CDDR(CAAAR BD)) ALUNOS)
										)
									)
									(CDAAR BD)
								)
								(CDAR BD)
							)
							(DESVINCULAR-ALUNO ALUNOS DISCIPLINAS TURMA (CDR BD))
						)
						(CONS (CAR BD) (DESVINCULAR-ALUNO ALUNOS DISCIPLINAS TURMA (CDR BD)))
					)
				)
			)
		)
	)
)
;<>


;REMOVE STUDENT FROM A SUBJECT
;CALLS "VERIFICA-DISCIPLINA" TO SEE IF THE SUBJECT DOESN'T NEED TO BE REMOVED
(DEFUN CANCELAR-MATRICULA (ALUNOS DISCIPLINAS TURMA BD)
	(VERIFICA-DISCIPLINA (DESVINCULAR-ALUNO ALUNOS DISCIPLINAS TURMA BD)))
;<>

;********************************************************************