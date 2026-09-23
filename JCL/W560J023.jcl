//W560J023 JOB (640W5600100W560J023,W100),'RTN W560D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W560    EXEC W560P023                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W560.W560D1.W56022A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W560.W560D1.W56022A(+1),CPU=10                            
//SYSIN            DD *                                                         
W56023-001                                                                      
US01                                                                            
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W560.W560D1.W56023A(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W560.W560D1.W56023A(+1),CPU=10                            
//SYSIN            DD *                                                         
W56023-002                                                                      
CA03                                                                            
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560J023                                         
