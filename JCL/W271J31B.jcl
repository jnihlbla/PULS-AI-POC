//W271J31B JOB (640W2710100W271J31B,W100),'RTN W271V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W271    EXEC W271P01B,                                                        
//             INDIN=W271.W271V2,                                               
//             INDUT=W271.W271V2                                                
//*                                                                             
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W271.W271V2.W2711B(+1),DISP=SHR                              
//SYSUT2   DD  DSN=WXTR.W271V2.W2711B(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J31B                                         
