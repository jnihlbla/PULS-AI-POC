//W510J096 JOB (640W5100100W510J096,W100),'RTN W510D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*************  VCCS SMARTFACTS********                                        
//EMPTY1  EXEC WEMPTST,DSIN=W510.W510D4.W5107SA(+0)                             
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//SFVCCS  EXEC WZ11SFAC,                                                        
//     DSIN=W510.W510D4.W5107SA(+0)                                             
//SYSIN DD *                                                                    
TABLE_NAME=VCCS_SAP_BOOKINGS                                                    
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510J096                                         
