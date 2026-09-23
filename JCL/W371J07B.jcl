//W371J07B JOB (640W3710100W371J07B,W100),'RTN W371V9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W371    EXEC W371P07B                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J07B                                         
