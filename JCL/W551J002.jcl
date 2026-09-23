//W551J002 JOB (650W5510100W551J002,W100),'RTN W551B7',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//W551    EXEC W551P002                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W551.W551B7.W55102A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W551.W551B7.W55102A(+1)                                   
//SYSIN           DD *                                                          
W55102-001                                                                      
W55102                                                                          
//    ENDIF                                                                     
//SOP     EXEC WSOPEND,PROCESS=W551J002                                         
