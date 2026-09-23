//W372J057 JOB (640W3710100W372J157,W100),'RTN W371P2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*ANVÄNDER SAMMA PROCEDUR SOM J057 I RUTIN W371P2                              
//W372    EXEC W372P057,                                                        
//             INDIN=W371.W371P2                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W372J057                                         
