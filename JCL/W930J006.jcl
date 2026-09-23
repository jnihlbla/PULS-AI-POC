//W930J006 JOB (640W9300100W930J006,W100),'RTN W930S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W930    EXEC W930P006                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W930.W930S2.W93003(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W930.W930S2.W93003(+1)                                    
//SYSIN           DD *                                                          
W93006-001                                                                      
W93006                                                                          
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W930J006                                         
