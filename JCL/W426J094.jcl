//W426J094 JOB (670W4260100W426J094,W100),'RTN W426V1',                         
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
//W01533.W01533D1 DD DSN='W426.W426V1.W42694(+0)',DISP=SHR                      
//                DD DSN='W426.W426V1.W42696(+0)',DISP=SHR                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W426J094                                         
