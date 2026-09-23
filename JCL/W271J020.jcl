//W271J020 JOB (670W2710100W271J020,W100),'RTN W271D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P020,                                                        
//             INDIN=W271.W271D1,                                               
//             INDUT=W271.W271D1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J020                                         
