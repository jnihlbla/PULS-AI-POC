//W221ZBSE JOB (670W2210100W221ZBSE,W100),'RTN W224V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* MQ TRANSFER EXTERNAL DELIVERY PLANS, NDC CHINA ANSK                         
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W224.W224V2.W2216D(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.EDI.VCC8161.VEDIUNB073.DELINS                               
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W221ZBSE                                         
