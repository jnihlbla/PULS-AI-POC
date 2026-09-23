//W611J089 JOB (640W6110100W611J089,W100),'RTN W611Y1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=9999                                        
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W611    EXEC W611P089                                                         
//*                                                                             
//*************  SMARTFACTS********                                             
//EMPTY1  EXEC WEMPTST,DSIN=W611.W611Y1.W611O9A(+1)                             
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//SFVCSC  EXEC WZ11SFAC,                                                        
//     DSIN=W611.W611Y1.W611O9A(+1)                                             
//SYSIN DD *                                                                    
TABLE_NAME=ITEMS_DELETED_FROM_INBOUND_HISTORY                                   
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//SOPEND  EXEC WSOPEND,PROCESS=W611J089                                         
