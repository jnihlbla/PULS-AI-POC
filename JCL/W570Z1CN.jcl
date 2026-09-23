//W570Z1CN JOB (650W5700100W570Z1CN,W100),'RTN W570D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//*   MQ ÖVERFÖRING SAP/R3, VCCN POSTER                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W570.W570D4.W57074                                   
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W570.W570D4.W57074                                          
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.SAP.FIPOSTING                                               
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W570Z1CN                                         
