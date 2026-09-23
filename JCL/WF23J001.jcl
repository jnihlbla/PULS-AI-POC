//WF23J001 JOB (640WF230100WF23J001,W100),'RTN WF23S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTF                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WF23    EXEC WF23P001                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WF23.WF23S1.WF2301(+1)                               
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//     EXEC WSOP                                                                
       ACTIVATE WF23JD01                                                        
//   ELSE                                                                       
//     EXEC PGM=IEFBR14                                                         
//DD1  DD DSN=WF23.WF23S1.WF2301(+1),DISP=(OLD,DELETE)                          
//   ENDIF                                                                      
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF23J001                                         
