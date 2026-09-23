//W221ZDSE JOB (670W2210100W221ZDSE,W100),'RTN W224V4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* MQ TRANSFER EXTERNAL DELIVERY PLANS, NDC NA-USA ANSK                        
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W224.W224V4.W2216D(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.EDI.VCC8161.VEDIUNB073.DELINS                               
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W221ZDSE                                         
