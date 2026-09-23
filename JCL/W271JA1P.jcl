//W271JA1P JOB (670W2710100W271JA1P,W100),'RTN W271B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W271    EXEC W271P01P,                                                        
//             INDIN=W271.W271B1,                                               
//             INDUT=W271.W271B1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271JA1P                                         
