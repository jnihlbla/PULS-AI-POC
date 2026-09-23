//W261J054 JOB (640W2610100W261J054,W100),'RTN W261V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W261    EXEC W261P054                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W261J054                                         
