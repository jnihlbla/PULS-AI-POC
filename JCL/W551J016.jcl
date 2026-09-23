//W551J016 JOB (670W5510100W551J016,W100),'RTN W551B9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W551    EXEC W551P016                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W551.W551B9.W55144X(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W551.W551B9.W55144X(+1)                                   
//SYSIN           DD *                                                          
W55116-001                                                                      
W55116                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W551J016                                         
