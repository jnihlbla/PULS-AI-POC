//W371J148 JOB (640W3710100W371J148,W100),'RTN W371V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*ANVÄNDER SAMMA PROCEDUR SOM J048 I RUTIN W371D3                              
//W371    EXEC W371P048,                                                        
//             INDIN=W371.W371V2                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J148                                         
