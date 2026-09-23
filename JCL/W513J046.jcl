//W513J046 JOB (650W5130100W513J046,W100),'RTN W500V1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W513    EXEC W513P046                                                         
//*                                                                             
//*************  SMARTFACTS********                                             
//EMPTY1  EXEC WEMPTST,DSIN=W513.W500V1.W513O6A(+1)                             
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//SFVCSC  EXEC WZ11SFAC,                                                        
//     DSIN=W513.W500V1.W513O6A(+1)                                             
//SYSIN DD *                                                                    
TABLE_NAME=PARTS_NEGATIVE_STOCKBALANCES                                         
DELIMITER=;                                                                     
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W513J046                                         
