//W221ZASE JOB (670W2210100W221ZASE,W100),'RTN W221D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* MQ TRANSFER EXTERNAL DELIVERY PLANS                                         
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W221.W221D5.W22162(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.EDI.VCC8161.VEDIUNB073.DELINS                               
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W221ZASE                                         
