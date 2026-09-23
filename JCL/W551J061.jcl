//W551J061 JOB (650W5510100W551J061,W100),'RTN W551B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W551    EXEC W551P061                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551J061                                         
