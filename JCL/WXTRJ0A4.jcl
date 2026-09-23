//WXTRJ0A4 JOB (640W0001000WXTRJ0A4,W100),'RTN WXTRD1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=00                                                  
//*                                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP0A4                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WXTRJ0A4                                         
