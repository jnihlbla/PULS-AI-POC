//W232J018 JOB (670W2320100W232J018,W100),'RTN W232V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W232    EXEC W232P018                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W232J018                                         
