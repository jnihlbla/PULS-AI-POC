//W335ZABE JOB (640W3350100W335ZABE,W100),'RTN W335V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*PRISFIL TILL XDMS                                                            
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//           DSIN=W335.W335V2.W33559(+0)                                        
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.XDMS.PRICEFILE                                              
¤MQMPROP PhysicalId=PULS_prices_%YYYYMMDD.TXT                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335ZABE                                         
