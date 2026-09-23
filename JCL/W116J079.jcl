//W116J079 JOB (640W1160100W116J079,W100),'RTN W116D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W116    EXEC W116P079                                                         
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W116.W116D4.W1167B(+1)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
//      EXEC WSOP                                                               
        ACTIVATE W116JD79                                                       
//    ELSE                                                                      
//      EXEC PGM=IEFBR14                                                        
//DD1   DD DSN=W116.W116D4.W1167B(+1),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J079                                         
