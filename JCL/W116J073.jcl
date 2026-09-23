//W116J073 JOB (640W1160100W116J073,W100),'RTN W116S7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W116    EXEC W116P073                                                         
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W116.W116S7.W11673(+1)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
//*   MQ STEP                                                                   
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116S7.W11673(+1)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=US                                                              
¤MQMPROP Vidb_Source=Full                                                       
¤MQMPROP LoadType=Full                                                          
/*                                                                              
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W116J073                                         
