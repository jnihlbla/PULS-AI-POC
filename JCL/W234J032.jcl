//W234J032 JOB (670W2340100W234J032,W100),'RTN W234V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W234    EXEC W234P032                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W234J032                                         
