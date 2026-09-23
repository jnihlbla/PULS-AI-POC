//W440J082 JOB (650W4400100W440J082,W100),'RTN W440V3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=150                                         
/*ROUTE XEQ LOCAL                                                               
//*+JBS BIND IMG0                                                               
/*ROUTE PRINT LOCAL                                                             
//W440    EXEC W440P082                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W440.W440V3.W44082(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W440.W440V3.W44082(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W440J082                                         
