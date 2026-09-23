//W510Z1S1 JOB (650W5100100W510Z1S1,W100),'RTN W510D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//*   MQ ÖVERFÖRING SAP/R3, VCCS POSTER                                         
//*************  SAP/R3 TRANSAKTIONER (GL/AR/AP)                                
//EMPTY1 EXEC WEMPTST,DSIN=W510.W510D5.W51074                                   
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W510.W510D5.W51074                                          
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.SAP.FIPOSTING                                               
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510Z1S1                                         
