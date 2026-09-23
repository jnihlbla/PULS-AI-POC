//WXTRJ0E1 JOB (640W0001000WXTRJ0E1,W100),'RTN WXTRV2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=00                                                  
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WXTR     EXEC WXTRP0E1                                                        
//*                                                                             
//END      EXEC WSOPEND,PROCESS=WXTRJ0E1                                        
