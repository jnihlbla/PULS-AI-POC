//W510Z8SE JOB (670W5100100W510Z8SE,W100),'RTN W510D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* TILL SAP MM POSTTYP 500                                                     
//*************  VCCS                                                           
//EMPTY1  EXEC WEMPTST,DSIN=WUT.W510D4.W51084(+0)                               
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//VCOM     EXEC W016P022,VCOM=W510Z8SE                                          
//W01622.W016ZZD1 DD DSN=WUT.W510D4.W51084(+0),DISP=SHR                         
//   ENDIF                                                                      
//*                                                                             
//* TILL SAP MM POSTTYP 500 US01                                                
//*************  VCCS                                                           
//EMPTY2  EXEC WEMPTST,DSIN=W510.W510D4.W51084A(+0)                             
//   IF (EMPTY2.T.RC = 0) THEN                                                  
//VCOM     EXEC W016P022,VCOM=W510Z8SE                                          
//W01622.W016ZZD1 DD DSN=W510.W510D4.W51084A(+0),DISP=SHR                       
//   ENDIF                                                                      
//*                                                                             
//*************  VCCS SMARTFACTS********                                        
//EMPTY3  EXEC WEMPTST,DSIN=W510.W510D4.W51084VC(+0)                            
//   IF (EMPTY3.T.RC = 0) THEN                                                  
//SFVCCS  EXEC WZ11SFAC,                                                        
//     DSIN=W510.W510D4.W51084VC(+0)                                            
//SYSIN DD *                                                                    
TABLE_NAME=VCCS_SAP_MM                                                          
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//*************  VCSC SMARTFACTS********                                        
//EMPTY4  EXEC WEMPTST,DSIN=W510.W510D4.W51084UC(+0)                            
//   IF (EMPTY4.T.RC = 0) THEN                                                  
//SFVCSC  EXEC WZ11SFAC,                                                        
//     DSIN=W510.W510D4.W51084UC(+0)                                            
//SYSIN DD *                                                                    
TABLE_NAME=VCSC_SAP_MM                                                          
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510Z8SE                                         
