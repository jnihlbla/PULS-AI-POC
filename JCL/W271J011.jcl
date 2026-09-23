//W271J011 JOB (670W2710100W271J011,W100),'RTN W271D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P011,                                                        
//             INDIN=W271.W271D2                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J011                                         
