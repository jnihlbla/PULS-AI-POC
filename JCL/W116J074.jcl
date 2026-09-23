//W116J074 JOB (640W1160100W116J074,W100),'RTN W116S8',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W116    EXEC W116P074                                                         
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W116.W116S8.W11674(+1)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
//*   MQ STEP                                                                   
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116S8.W11674(+1)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=CA                                                              
¤MQMPROP Vidb_Source=Full                                                       
¤MQMPROP LoadType=Full                                                          
/*                                                                              
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W116J074                                         
