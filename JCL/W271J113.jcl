//W271J113 JOB (670W2710100W271J113,W100),'RTN W271V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P013,                                                        
//             INDIN=W271.W271V2                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J113                                         
