//W510J081 JOB (640W5100100W510J081,W100),'RTN W510D4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W510    EXEC W510P081                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W510.W510D4.W5107U(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W510.W510D4.W5107U(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W510.W510D4.W5107V(+1)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W510.W510D4.W5107V(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//EMPTY3 EXEC WEMPTST,DSIN=W510.W510D4.W5107W(+1)                               
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W510.W510D4.W5107W(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//EMPTY4 EXEC WEMPTST,DSIN=W510.W510D4.W5107X(+1)                               
//    IF (EMPTY4.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W510.W510D4.W5107X(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510J081                                         
