//W216J001 JOB (670W2160100W216J001,W100),'RTN W216S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W216    EXEC W216P001                                                         
//*                                                                             
//WAIT    EXEC WWAIT,SECONDS=10                                                 
//*                                                                             
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W216.W216S1.W21611(+1),DISP=SHR                              
//SYSUT2   DD  DSN=W216.W216S1.W21612(+0),                                      
//             DISP=(MOD,KEEP),                                                 
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W216J001                                         
