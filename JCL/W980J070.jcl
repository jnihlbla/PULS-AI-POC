//W980J070 JOB (640W0090100W980J070,W100),'RTN W980P1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJESD                                                             
/*ROUTE PRINT NJOVC                                                             
//W980    EXEC W980P070,INDGRND=W.PROD                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W980J070                                         
