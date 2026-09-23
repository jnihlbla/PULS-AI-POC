//W252J016 JOB (640W2520100W252J016,W100),'RTN W252D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W252    EXEC W252P016                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W252.W252D1.W25216(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W252.W252D1.W25216(+1)                                    
//SYSIN           DD *                                                          
W25216-001                                                                      
W25216                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W252J016                                         
