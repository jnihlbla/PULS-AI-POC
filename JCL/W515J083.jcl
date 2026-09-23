//W515J083 JOB (640W5150100W515J083,W100),'RTN W515D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*************  VCIN SMARTFACTS********                                        
//EMPTY1  EXEC WEMPTST,DSIN=W515.W515D3.W5157S(+0)                              
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//SFVCCS  EXEC WZ11SFAC,                                                        
//     DSIN=W515.W515D3.W5157S(+0)                                              
//SYSIN DD *                                                                    
TABLE_NAME=VCSC_SAP_BOOKINGS                                                    
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W515J083                                         
