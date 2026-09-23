//W512J06E JOB (640W5120100W512J06E,W100),'RTN W512M2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W512    EXEC W512P06E                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W512.W512M2.W5126E(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W512.W512M2.W5126E(+1)                                    
//SYSIN            DD *                                                         
W5126E-001                                                                      
W5126E                                                                          
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W512J06E                                         
