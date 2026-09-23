//W517J010 JOB (650W5170100W517J010,W100),'RTN W510D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//       EXEC W517P010                                                          
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W517.W510D1.W51710(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W517.W510D1.W51710(+1)                                    
//SYSIN           DD *                                                          
W51710-001                                                                      
W51710                                                                          
//    ENDIF                                                                     
//SOP     EXEC WSOPEND,PROCESS=W517J010                                         
