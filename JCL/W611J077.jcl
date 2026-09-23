//W611J077 JOB (640W6110100W611J077,W100),'RTN W611V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST6                                                     
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W611    EXEC W611P077                                                         
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W611.W611V1.W61177B(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W611.W611V1.W61177B(+1)                                   
//      ENDIF                                                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J077                                         
