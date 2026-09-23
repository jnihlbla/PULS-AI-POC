//W570J063 JOB (640W5700100W570J063,W100),'RTN W570D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W570    EXEC W570P063                                                         
//*                                                                             
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W570.W570D2.W57066(+1),DISP=SHR                              
//SYSUT2   DD  DSN=W570.W570D2.W57066B(+0),                                     
//             DISP=(MOD,KEEP),                                                 
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W570J063                                         
