//W476J029 JOB (670W4760100W476J029,W100),'RTN W476D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W476    EXEC W015P037,                                                        
//             DSIN=W476.W476D1.W47629B(+0)                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J029                                         
