//W231J001 JOB (640W2310100W231J001,W100),'RTN W231P1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W231    EXEC W231P001                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W231J001                                         
