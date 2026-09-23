//WXTRJ1A3 JOB (640W0001000WXTRJ1A3,W100),'RTN WXTRV2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=00                                                  
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP0A3                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WXTRJ1A3                                         
