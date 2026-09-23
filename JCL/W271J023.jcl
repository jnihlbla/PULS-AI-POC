//W271J023 JOB (670W2710100W271J023,W100),'RTN W271D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W271    EXEC W271P023,                                                        
//             INDIN=W271.W271D2,                                               
//             INDUT=W271.W271D2                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J023                                         
