//W512J005 JOB (650W5120100W512J005,W100),'RTN W500M1',                         
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
//*****************************************************************             
//* MAIL TILL EKONOMI VIA D&P, STOCKVALUE/DC/PS                                 
//* W512.W500M1.W51205                                                          
//*****************************************************************             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W512.W500M1.W51205(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W512.W500M1.W51205(+0)                                    
//SYSIN           DD *                                                          
W51205-001                                                                      
W51205                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W512J005                                         
