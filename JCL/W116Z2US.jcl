//W116Z2US JOB (640W1160100W116Z2US,W100),'RTN W116D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W116.W116D2.W11636(+0)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
//*   MQ STEP                                                                   
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D2.W11636(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=US                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W116Z2US                                         
