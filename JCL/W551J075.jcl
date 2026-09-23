//W551J075 JOB (640W5510100W551J075,W100),'RTN W551B1',                         
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
//W551    EXEC W551P075                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W551.W551B1.W55175A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W551.W551B1.W55175A(+1)                                   
//SYSIN           DD *                                                          
W55175-001                                                                      
W55175                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W551.W551B1.W55175B(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W551.W551B1.W55175B(+1)                                   
//SYSIN           DD *                                                          
W55175-002                                                                      
W55175                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY3 EXEC WEMPTST,DSIN=W551.W551B1.W55175C(+1)                              
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W551.W551B1.W55175C(+1)                                   
//SYSIN           DD *                                                          
W55175-003                                                                      
W55175                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551J075                                         
