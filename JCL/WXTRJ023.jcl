//WXTRJ023 JOB (640WXTR0100WXTRJ023,W100),'RTN WXTRD3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* INPUT FILE IS ZIPPED IN W011J060 JOB                                        
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W011.QASE.W01164X(+0)                                       
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=packaginginformation%YYYYMMDD.ZIP                           
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ023                                         
