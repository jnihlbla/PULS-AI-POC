//W476J047 JOB (670W4760100W476J047,W100),'RTN W476V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W476    EXEC W015P037,                                                        
//             DSIN=W476.W476V2.W47647(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J047                                         
