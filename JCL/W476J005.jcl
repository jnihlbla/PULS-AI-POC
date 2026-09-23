//W476J005 JOB (670W4760100W476J005,W100),'RTN W476D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W476    EXEC W476P105                                                         
//W47605.W47605D1 DD DSN=W.QASE.CONSTANT(W476BMPN)                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J005                                         
