//W540J010 JOB (670W5400100W540J010,W100),'RTN W553V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W540    EXEC W540P010                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W540J010                                         
