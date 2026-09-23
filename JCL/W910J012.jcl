//W910J012 JOB (640W9100100W910J012,W100),'RTN W910D1',                         
//             USER=?,PASSWORD=?,                                               
//         CLASS=K                                                              
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST9                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W910    EXEC W910P012                                                         
//SOP     EXEC WSOPEND,PROCESS=W910J012                                         
/*                                                                              
