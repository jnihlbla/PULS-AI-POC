//W611J091 JOB (640W6110100W611J091),'RTN W611V1',                              
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//W611     EXEC W611P091                                                        
//*                                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W611.W611V1.W61191B(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W611.W611V1.W61191B(+1)                                   
//      ENDIF                                                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J091                                         
