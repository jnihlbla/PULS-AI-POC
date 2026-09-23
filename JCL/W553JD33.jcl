//W553JD33 JOB (640W5530100W553JD33,W100),'RTN W553V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*************PRICE ADJUSTMENT TO SNOWFLAKE****                                
//EMPTY1  EXEC WEMPTST,DSIN=W553.W553V1.W55333(+0)                              
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//SFVCCS  EXEC WZ11SFAC,                                                        
//     DSIN=W553.W553V1.W55333(+0)                                              
//SYSIN DD *                                                                    
TABLE_NAME=VCCS_PRICE_ADJUSTMENTS                                               
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W553JD33                                         
