//W221J198 JOB (640W2210100W221J198,W100),'RTN W221V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W221.W221V3.W22198(+0),DISP=SHR                              
//SYSUT2   DD  DSN=&&W22198X,                                                   
//             DISP=(NEW,PASS,DELETE),                                          
//             MGMTCLAS=NOBACKUP,DATACLAS=PSEN                                  
//*                                                                             
//TZIP    EXEC WZ11TZIP,                                                        
//            DSIN=&&W22198X,                                                   
//            DSOUTZIP=W221.W221V3.W22198X(+1),                                 
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=W22198X.CSV                                               
//*                                                                             
//WQSEN   EXEC WZ11P023,ABSADDRS=CARPARTS.AZURE.DATA,                           
//             DSIN=W221.W221V3.W22198X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤MQMPROPPhysicalId=deliverynotice%YYYYMMDD.ZIP                                  
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W221J198                                         
