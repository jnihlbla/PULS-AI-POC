//W571J003 JOB (670W5710100W571J003,W100),'RTN W571B2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W571    EXEC W571P003                                                         
//*                                                                             
&IDDC                                                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W571J003                                         
