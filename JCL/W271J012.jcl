//W271J012 JOB (640W2710100W271J012,W100),'RTN W271D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//*+JBS BIND IMG0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P012,REFILL=D2,                                              
//             INDUT2=W271.W271D2                                               
//*                                                                             
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W412.IN.W41210(+1),DISP=SHR                                  
//SYSUT2   DD  DSN=WXTR.W271D2.W41210(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J012                                         
