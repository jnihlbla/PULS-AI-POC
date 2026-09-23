//W271J436 JOB (670W2710100W271J436,W100),'RTN W271V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P036,                                                        
//             INDIN=W271.W271V2,                                               
//             INDUT=W271.W271V2                                                
//*                                                                             
//W27136.W27136D1 DD DSN=W271.W271V2.W2712B(+0)                                 
//                DD DUMMY                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J436                                         
