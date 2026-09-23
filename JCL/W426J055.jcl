//W426J055 JOB (640W4260100W426J055,W100),'RTN W426D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W426    EXEC W426P055                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W426.W426D2.W42655(+1)                               
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//     EXEC WSOP                                                                
       ACTIVATE W426JD55                                                        
//   ELSE                                                                       
//     EXEC PGM=IEFBR14                                                         
//DD1  DD DSN=W426.W426D2.W42655(+1),DISP=(OLD,DELETE)                          
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W426J055                                         
