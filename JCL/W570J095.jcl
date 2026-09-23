//W570J095 JOB (640W5700100W570J095,W100),'RTN W570D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*************  VCSC SMARTFACTS*********                                       
//EMPTY1  EXEC WEMPTST,DSIN=W570.W570D3.W5702S(+0)                              
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//SFVCCS  EXEC WZ11SFAC,                                                        
//     DSIN=W570.W570D3.W5702S(+0)                                              
//SYSIN DD *                                                                    
TABLE_NAME=VCSC_SAP_BOOKINGS                                                    
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//*************  VCCN SMARTFACTS********                                        
//EMPTY2  EXEC WEMPTST,DSIN=W570.W570D3.W5707S(+0)                              
//   IF (EMPTY2.T.RC = 0) THEN                                                  
//SFVCCS  EXEC WZ11SFAC,                                                        
//     DSIN=W570.W570D3.W5707S(+0)                                              
//SYSIN DD *                                                                    
TABLE_NAME=VCSC_SAP_BOOKINGS                                                    
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W570J095                                         
