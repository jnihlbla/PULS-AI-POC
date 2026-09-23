//W426J092 JOB (670W4260100W426J092,W100),'RTN W426D2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W015    EXEC W015P033                                                         
//W01533.W01533D1 DD DSN='W426.W426D2.W42692(+0)',DISP=SHR                      
//                DD DSN='W426.W426D2.W42695(+0)',DISP=SHR                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W426J092                                         
