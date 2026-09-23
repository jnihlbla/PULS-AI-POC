//W541J010 JOB (650W5410100W541J010,W100),'RTN W500V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND  IMG0                                                              
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W541    EXEC W541P010                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W541.W500V1.W54110(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W541.W500V1.W54110(+1)                                    
//    ENDIF                                                                     
//SOP     EXEC WSOPEND,PROCESS=W541J010                                         
