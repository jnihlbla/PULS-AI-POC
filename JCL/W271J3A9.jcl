//W271J3A9 JOB (640W2710100W271J3A9,W100),'RTN W271DB',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//W271    EXEC W271P013,                                                        
//             INDIN=W271.W271DB                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J3A9                                         
