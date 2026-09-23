//W551J090 JOB (640W5510100W551J090,W100),'RTN WYR001',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W551    EXEC W551P090                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W551.WYR001.W55190A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W551.WYR001.W55190A(+1)                                   
//SYSIN           DD *                                                          
W55190-001                                                                      
W55190                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W551.WYR001.W55190B(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W551.WYR001.W55190B(+1)                                   
//SYSIN           DD *                                                          
W55190-002                                                                      
W55190                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY3 EXEC WEMPTST,DSIN=W551.WYR001.W55190C(+1)                              
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W551.WYR001.W55190C(+1)                                   
//SYSIN           DD *                                                          
W55190-003                                                                      
W55190                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551J090                                         
