//W236J049 JOB (670W2360100W236J049,W100),'RTN W236D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W015    EXEC W015P033,                                                        
//             DSIN=W236.W236D1.W23649(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W236J049                                         
