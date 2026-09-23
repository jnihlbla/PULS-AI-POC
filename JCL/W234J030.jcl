//W234J030 JOB (640W2340100W234J030,W100),'RTN W234V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W234    EXEC W234P030                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W234J030                                         
