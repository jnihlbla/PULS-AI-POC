//W515Z1IN JOB (650W5100100W515Z1IN,W100),'RTN W515D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//*   MQ ÖVERFÖRING SAP/R3, VCIN POSTER                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W515.W515D4.W51574                                   
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W515.W515D4.W51574                                          
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.SAP.FIPOSTING                                               
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W515Z1IN                                         
