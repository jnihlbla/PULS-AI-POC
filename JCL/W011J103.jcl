//W011J103 JOB (640W0110100W011J103,W100),'RTN W011D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W011    EXEC W011P103                                                         
//*                                                                             
//SORT2    EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=&INDRTE..CONSTANT(W011PD3X),DISP=SHR                         
//SORTIN   DD  DSN=&&W01103X,DISP=(OLD,DELETE,DELETE)                           
//SORTOUT  DD  DSN=&&W01103S,                                                   
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEB,MGMTCLAS=NOBACKUP                                  
//*                                                                             
//TZIP    EXEC WZ11TZIP,                                                        
//            DSIN=&&W01103S,                                                   
//            DSOUTZIP=W011.W011D1.W01103X(+1),                                 
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=W01103X.CSV                                               
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W011.W011D1.W01103X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=supersession%YYYYMMDD.ZIP                                   
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J103                                         
