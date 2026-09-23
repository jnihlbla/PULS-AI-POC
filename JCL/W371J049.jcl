//W371J049 JOB (640W3710100W371J049,W100),'RTN W371M1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P049                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J049                                         
