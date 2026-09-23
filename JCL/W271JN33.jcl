//W271JN33 JOB (670W2710100W271JN33,W100),'RTN W271DA',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P033,                                                        
//             INDIN=W271.DC51                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271JN33                                         
