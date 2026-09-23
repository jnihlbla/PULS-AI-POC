//W476Z1PO JOB (670W4760100W476Z1PO,W100),'RTN W476S4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W476.W476S4.W47643A(+0)                             
//*                                                                             
//   IF (EMPTYT.T.RC = 4) THEN                                                  
//DEL1    EXEC PGM=IEFBR14                                                      
//DD      DD DSN=W476.W476S4.W47643A(+0),DISP=(OLD,DELETE)                      
//   ELSE                                                                       
//COPYLOG  EXEC PGM=IEFBR14                                                     
//SYSPRINT DD SYSOUT=*                                                          
//SYSUT1   DD DSN=W476.W476S4.W47643A(+0),DISP=SHR                              
//SYSUT2   DD DSN=W476.W476S4MQ.W47643A(+1),                                    
//            DISP=(NEW,CATLG,DELETE),DATACLAS=PSEN,                            
//            DCB=(DSORG=PS,RECFM=VB,LRECL=147),MGMTCLAS=DEL2                   
//SYSIN    DD DUMMY                                                             
//SYSOUT   DD SYSOUT=*                                                          
//SYSUDUMP DD SYSOUT=*                                                          
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//ICONV    EXEC PGM=EDCICONV,REGION=7M,                                         
//            PARM=('FROMCODE(IBM-278),TOCODE(IBM-1047)')                       
//STEPLIB  DD DSNAME=SYS1.CEE.SCEERUN,DISP=SHR                                  
//SYSUT1   DD DISP=SHR,DSN=W476.W476S4.W47643A(+0)                              
//SYSUT2   DD DSNAME=W476.W476S4MQ.W47643A(+1),                                 
//            DISP=(MOD,CATLG,DELETE)                                           
//SYSPRINT DD SYSOUT=*                                                          
//SYSIN    DD DUMMY                                                             
//* SEND DELIVERYSCHEDULES FOR LOCAL PURCHASE ORDERS-HIT05                      
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W476.W476S4MQ.W47643A(+1)                                   
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.EDI.EDIFILESGATEWAY.HIT05                                   
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476Z1PO                                         
