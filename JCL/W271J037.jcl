//W271J037 JOB (640W2710100W271J037,W100),'RTN W271D9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P037                                                         
//*                                                                             
//ACT     EXEC WSOP,COMMAND='ACTIVATE W271J137'                                 
//SOPEND  EXEC WSOPEND,PROCESS=W271J037                                         
