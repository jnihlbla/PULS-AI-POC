//W371J04R JOB (640W3710100W371J04R,W100),'RTN W371D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P04R                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J04R                                         
