//W371J04I JOB (640W3710100W371J04I,W100),'RTN W371V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P04I                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J04I                                         
