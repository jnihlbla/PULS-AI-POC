//W271J32F JOB (670W2710100W271J32F,W100),'RTN W271DA',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P02F,                                                        
//             INDIN=W271.W271DA                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J32F                                         
