//W522JD73 JOB (670W5100100W522JD73,W100),'RTN W522D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*************  VAT SMARTFACTS********                                         
//EMPTY1  EXEC WEMPTST,DSIN=W522.W522D1.W52273A(+0)                             
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//SFVCSC  EXEC WZ11SFAC,                                                        
//     DSIN=W522.W522D1.W52273A(+0)                                             
//SYSIN DD *                                                                    
TABLE_NAME=VCCS_INTRASTAT                                                       
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W522JD73                                         
