//W910J013 JOB (640W9100100W910J013,W100),'RTN W910D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W910    EXEC W910P013                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910J013                                         
