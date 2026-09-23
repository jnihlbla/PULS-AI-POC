//WXTRJ002 JOB (640W0001000WXTRJ002,W100),'RTN WXTRV2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=00                                                  
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP002                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WXTRJ002                                         
