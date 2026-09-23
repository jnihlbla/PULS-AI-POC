//W561J083 JOB (640W5610100W561J083,W100),'RTN W561D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*************  VCUS SMARTFACTS********                                        
//EMPTY1  EXEC WEMPTST,DSIN=W561.W561D3.W5617S(+0)                              
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//SFVCCS  EXEC WZ11SFAC,                                                        
//     DSIN=W561.W561D3.W5617S(+0)                                              
//SYSIN DD *                                                                    
TABLE_NAME=VCSC_SAP_BOOKINGS                                                    
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W561J083                                         
