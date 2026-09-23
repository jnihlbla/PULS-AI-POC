//W271J123 JOB (670W2710100W271J123,W100),'RTN W271V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P023,                                                        
//             INDIN=W271.W271V2,                                               
//             INDUT=W271.W271V2                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J123                                         
