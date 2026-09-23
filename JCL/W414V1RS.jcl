//W414V1RS JOB (640W4140100W414V1RS,W100),'RTN W414V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W414V1                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=W414.W414D3.W41415,                                             
//           T1=W414.W414V1.W41415,RF1=FB,LR1=0039,                             
//*                                                                             
//           F2=W414.W414D1.W41418,                                             
//           T2=W414.W414V1.W41418,RF2=FB,LR2=0125,                             
//*                                                                             
//           F3=W414.W414D1.W41419,                                             
//           T3=W414.W414V1.W41419,RF3=FB,LR3=0211                              
//*                                                                             
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W414.W414V1.W41418,DISP=SHR                                  
//SYSUT2   DD  DSN=WXTR.VORLOG.WEEKLY(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//COPY2   EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W414.W414V1.W41419,DISP=SHR                                  
//SYSUT2   DD  DSN=WXTR.XDCLOG.WEEKLY(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W414V1RS                                         
