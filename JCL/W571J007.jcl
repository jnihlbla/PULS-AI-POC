//W571J007 JOB (640W5710100W571J007,W100),'RTN W571B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W571    EXEC W571P007                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W571.W571B1.W57107A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B1.W57107A(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W571.W571B1.W57107B(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B1.W57107B(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//EMPTY3 EXEC WEMPTST,DSIN=W571.W571B1.W57107C(+1)                              
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B1.W57107C(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//EMPTY4 EXEC WEMPTST,DSIN=W571.W571B1.W57107D(+1)                              
//    IF (EMPTY4.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B1.W57107D(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//EMPTY5 EXEC WEMPTST,DSIN=W571.W571B1.W57107E(+1)                              
//    IF (EMPTY5.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W571.W571B1.W57107E(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W571J007                                         
