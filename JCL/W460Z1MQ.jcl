//W460Z1MQ JOB (670W4600100W460Z1MQ,W100),'RTN W460S3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W460.W460S3.W46055(+0)                              
//*                                                                             
//     IF (EMPTYT.T.RC = 0) THEN                                                
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W460.W460S3.W46055(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.ORDERRECEIVECONFIRMATION                               
¤MQMPROP Market=&COUNTRYX2                                                      
/*                                                                              
//     ENDIF                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W460Z1MQ                                         
