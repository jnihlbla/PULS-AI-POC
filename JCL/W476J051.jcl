//W476J051 JOB (670W4760100W476J051,W100),'RTN W476D5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W476    EXEC W476P051                                                         
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J051                                         
