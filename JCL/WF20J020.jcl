//WF20J020 JOB (640WF200100WF20J020,W100),'RTN WF20S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//GENER01 EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=WF20.WF20S2.WF2031(+0),DISP=SHR                              
//SYSUT2   DD  DSN=WF20.WF20S2.WF2031A(+1),DISP=(NEW,CATLG,DELETE),             
//             DATACLAS=PSEN,MGMTCLAS=NOBACKUP                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF20J020                                         
