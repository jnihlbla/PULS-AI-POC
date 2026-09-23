//W271JA13 JOB (670W2710100W271JA13,W100),'RTN W271B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P013,                                                        
//             INDIN=W271.W271B1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271JA13                                         
