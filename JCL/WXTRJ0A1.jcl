//WXTRJ0A1 JOB (640W0001000WXTRJ0A1,W100),'RTN W479V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=00                                                  
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WXTR     EXEC WXTRP0A1                                                        
//*                                                                             
//END      EXEC WSOPEND,PROCESS=WXTRJ0A1                                        
