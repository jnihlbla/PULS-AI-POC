//W215J008 JOB (670W2150100W215J008,W100),'RTN W215S3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W215    EXEC W215P008                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W215J008                                         
