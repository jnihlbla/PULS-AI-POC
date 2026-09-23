//W476J038 JOB (640W4760100W476J038,W100),'RTN W476D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTÖ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W476    EXEC W476P038,                                                        
//         DSIN=W476.W476D1.W47637(+0),                                         
//         DSUT=WUT.W476D1.W476JP(+1)                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J038                                         
