//W560J052 JOB (640W5600100W560J052,W100),'RTN W560M1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W560    EXEC W560P052                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W560.W560M1.W56052S(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W560.W560M1.W56052S(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W560.W560M1.W56052K(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W560.W560M1.W56052K(+1)                                   
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W560J052                                         
