//W510J084 JOB (650W5100100W510J084,W100),'RTN W510D4',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
//*+JBS BIND IMG0                                                               
/*ROUTE PRINT LOCAL                                                             
//W510    EXEC W510P084                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W510.W510D4.W51085(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W510.W510D4.W51085(+1)                                    
//SYSIN           DD *                                                          
W51085-001                                                                      
W51085                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W510J084                                         
