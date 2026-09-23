//W611J008 JOB (640W6110100W611J008,W100),'RTN W611S9',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W611    EXEC W611P008                                                         
//*                                                                             
//EMPTY1  EXEC WEMPTST,DSIN=W611.W611S9.W611083(+1)                             
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W611.W611S9.W611083(+1)                                   
//SYSIN           DD *                                                          
W61108-001                                                                      
W61108                                                                          
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W611J008                                         
