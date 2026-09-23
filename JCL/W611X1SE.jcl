//W611X1SE JOB (540W6110100W611X1SE,W100),'RTN W611X1',                         
//             CLASS=K                                                          
/*JOBPARM TIME=1,LINES=5,FORMS=1800,LINECT=0                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP,COMMAND='ORDER W611X1'                                      
