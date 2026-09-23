//WXTRJ025 JOB (640WXTR0100WXTRJ025,W100),'RTN WXTRD3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=WXTR.WXTRD1.WXTRA5(+0),DISP=SHR                              
//SYSUT2   DD  DSN=&&WXTRA5X,                                                   
//             DISP=(NEW,PASS,DELETE),                                          
//             MGMTCLAS=NOBACKUP,DATACLAS=PSEN                                  
//*                                                                             
//TZIP    EXEC WZ11TZIP,                                                        
//            DSIN=&&WXTRA5X,                                                   
//            DSOUTZIP=WXTR.WXTRD3.WXTRA5X(+1),                                 
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=WXTRA5X.CSV                                               
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=WXTR.WXTRD3.WXTRA5X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=invoicedlines%YYYYMMDD.ZIP                                  
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ025                                         
