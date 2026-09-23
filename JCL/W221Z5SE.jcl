//W221Z5SE JOB (640W2210100W221Z5SE,W100),'RTN W221D8',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* SEND DELIVERYSCHEDULES FOR LOCAL PURCHASE ORDERS                            
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W221.W221D8.W2216D(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.EDI.VCC8161.VEDIUNB073.DELINS                               
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W221Z5SE                                         
