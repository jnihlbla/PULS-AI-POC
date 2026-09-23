//W271J094 JOB (640W2710100W271J094,W100),'RTN W271V4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//W271    EXEC W271P094                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W271.W271V4.W27194(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W271.W271V4.W27194(+1)                                    
//* SYSIN USED TO IGNORE THE CONTROL CHARACTERS IN VBA FILE                     
//SYSIN DD *                                                                    
1                                                                               
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J094                                         
