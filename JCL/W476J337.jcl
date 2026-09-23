//W476J337 JOB (640W4760100W476J337,W100),'RTN W476D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W476    EXEC W476P037,                                                        
//         DSIN=W476.W476D1.W47638(+0),                                         
//         DSUT=WUT.W476D1.W476AU(+1)                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J337                                         
