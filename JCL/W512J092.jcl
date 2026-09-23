//W512J092 JOB (640W5120100W512J092,W100),'RTN W512M4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTF                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W512    EXEC W512P092                                                         
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS REPORTS FROM THE                               
//*  INPUT FILE W512.W512M4.W51292                                              
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W512.W512M4.W51292(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W512.W512M4.W51292(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W512J092                                         
