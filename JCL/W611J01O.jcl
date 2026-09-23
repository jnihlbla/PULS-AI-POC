//W611J01O JOB (640W6110100W611J01O,W100),'RTN W611V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*W611    EXEC W611P01O                                                        
//*                                                                             
//*************R34-TRANSACTIONS PERFORMED LAST WEEK**W61124-0XX**               
//EMPTY1  EXEC WEMPTST,DSIN=W611.W611V1.W611O1A(+0)                             
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//SFVCCS  EXEC WZ11SFAC,                                                        
//     DSIN=W611.W611V1.W611O1A(+0)                                             
//SYSIN DD *                                                                    
TABLE_NAME=VCCS_STOCKTAKING_R34_TRANSACTIONS                                    
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J01O                                         
