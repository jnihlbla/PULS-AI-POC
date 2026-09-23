//W476J105 JOB (670W4760100W476J105,W100),'RTN W476S3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W476    EXEC W476P105                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J105                                         
