//W271J074 JOB (640W2710100W271J074,W100),'RTN W271V7',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P074                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W271.W271V7.W27174(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W271.W271V7.W27174(+1)                                    
//SYSIN           DD *                                                          
W27174-001                                                                      
W27174                                                                          
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W271J074                                         
