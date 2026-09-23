//W233ZBSE JOB (640W2330100W233ZBSE,W100),'RTN W233V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*                                                                             
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W233.W233V3.W23326(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.SPAREPARTVOLUME                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W233ZBSE                                         
