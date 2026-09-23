//W910J041 JOB (640W9100100W910J041,W100),'RTN W91054',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W910    EXEC W910P041                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910J041                                         
