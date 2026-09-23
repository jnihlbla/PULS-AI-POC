//W611S1OK JOB (540W6110100W611S1OK,W100),'RTN W611S1',                         
//             CLASS=K                                                          
/*JOBPARM TIME=1,LINES=5,FORMS=1800,LINECT=0                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*SOP     EXEC WSOP,COMMAND='ORDER W611S1'                                     
//*                                                                             
//ABE     IF ABEND THEN                                                         
//SOP     EXEC WSOP,COMMAND='ABEND W611S1ER'                                    
//ABE     ENDIF                                                                 
