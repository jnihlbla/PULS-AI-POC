//W092X3SE JOB (540W0920100W092X3SE,W100),'RTN W092X3',                         
//             CLASS=K                                                          
/*JOBPARM TIME=1,LINES=5,FORMS=1800,LINECT=0                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP,COMMAND='ORDER W092X3'                                      
