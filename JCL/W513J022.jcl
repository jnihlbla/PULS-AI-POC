//W513J022 JOB (640W5130100W513J022,W100),'RTN W510D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W513    EXEC W513P022                                                         
//*                                                                             
//*************STOCKTAKING ADJUSTEMENTS RAW DATA*****************               
//EMPTY1  EXEC WEMPTST,DSIN=W513.W510D2.W51337(+1)                              
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//SFVCCS  EXEC WZ11SFAC,                                                        
//     DSIN=W513.W510D2.W51337(+1)                                              
//SYSIN DD *                                                                    
TABLE_NAME=VCCS_STOCKTAKING_ADJUSTMENTS_DATA                                    
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W513J022                                         
