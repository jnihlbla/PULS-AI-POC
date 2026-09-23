//W476J027 JOB (670W4760100W476J027,W100),'RTN W476D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W476    EXEC W015P037,                                                        
//             DSIN=W476.W476D3.W47627(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J027                                         
