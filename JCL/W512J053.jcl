//W512J053 JOB (650W5120100W512J053,W100),'RTN W512M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W512.W512M1.W51252(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W512.W512M1.W51252(+0)                                    
//SYSIN           DD *                                                          
W51253-001                                                                      
W51253                                                                          
//    ENDIF                                                                     
//EMPTY2 EXEC WEMPTST,DSIN=W512.W512M1.W51253(+0)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W512.W512M1.W51253(+0)                                    
//SYSIN           DD *                                                          
W51253-002                                                                      
W51253                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W512J053                                         
