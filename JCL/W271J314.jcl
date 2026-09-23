//W271J314 JOB (670W2710100W271J314,W100),'RTN W271DB',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P014,                                                        
//             INDIN=W271.W271DB                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J314                                         
