//W551J054 JOB (650W5510100W551J054,W100),'RTN W551B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=999                                         
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//W551    EXEC W551P054                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W551.W551B1.W55154F(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W551.W551B1.W55154F(+1)                                   
//SYSIN           DD *                                                          
W55154-001                                                                      
W55154                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W551.W551B1.W55154G(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W551.W551B1.W55154G(+1)                                   
//SYSIN           DD *                                                          
W55154-002                                                                      
W55154                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551J054                                         
