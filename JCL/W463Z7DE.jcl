//W463Z7DE JOB (670W4630100W463Z7DE,W100),'RTN W463V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W463.W463V1.W46368(+0)                              
//*                                                                             
//   IF (EMPTYT.T.RC = 4) THEN                                                  
//DEL1    EXEC PGM=IEFBR14                                                      
//DD      DD DSN=W463.W463V1.W46368(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//COPYLOG  EXEC PGM=IEFBR14                                                     
//SYSPRINT DD SYSOUT=*                                                          
//SYSUT1   DD DSN=W463.W463V1.W46368(+0),DISP=SHR                               
//SYSUT2   DD DSN=W463.W463V1MQ.W46368(+1),                                     
//            DISP=(NEW,CATLG,DELETE),DATACLAS=PSEN,                            
//            DCB=(DSORG=PS,RECFM=FB,LRECL=224),MGMTCLAS=DEL2                   
//SYSIN    DD DUMMY                                                             
//SYSOUT   DD SYSOUT=*                                                          
//SYSUDUMP DD SYSOUT=*                                                          
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//ICONV    EXEC PGM=EDCICONV,REGION=7M,                                         
//            PARM=('FROMCODE(IBM-278),TOCODE(IBM-1047)')                       
//STEPLIB  DD DSNAME=SYS1.CEE.SCEERUN,DISP=SHR                                  
//SYSUT1   DD DISP=SHR,DSN=W463.W463V1.W46368(+0)                               
//SYSUT2   DD DSNAME=W463.W463V1MQ.W46368(+1),                                  
//            DISP=(MOD,CATLG,DELETE)                                           
//SYSPRINT DD SYSOUT=*                                                          
//SYSIN    DD DUMMY                                                             
//* TILL IMS ADVANSYS TYSKLAND, DEALER INFO  *                                  
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W463.W463V1MQ.W46368(+1)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.EDI.EDIFILESGATEWAY.DEALER                                  
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463Z7DE                                         
