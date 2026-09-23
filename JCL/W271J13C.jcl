//W271J13C JOB (670W2710100W271J13C,W100),'RTN W271V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P03C,                                                        
//             INDIN=W271.W271V1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J13C                                         
