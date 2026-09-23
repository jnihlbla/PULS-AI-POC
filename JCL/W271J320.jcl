//W271J320 JOB (670W2710100W271J320,W100),'RTN W271DA',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P020,                                                        
//             INDIN=W271.W271DA,                                               
//             INDUT=W271.W271DA                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J320                                         
