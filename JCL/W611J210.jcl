//W611J210 JOB (640W6110100W611J210,W100),'RTN W611V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W611    EXEC W611P010                                                         
//W61110.W61110D2 DD DUMMY                                                      
//*                                                                             
//COPYFIL EXEC PGM=V16459,PARM='ICEGENER/3/'                                    
//SYSPRINT DD SYSOUT=*                                                          
//SYSUT1   DD DSN=WXTR.W611D1.W61110(+1),DISP=SHR                               
//SYSUT2   DD DSN=W611.W611V1.W61111(+1),                      -LIM(1)          
//            DISP=(NEW,CATLG,DELETE),                                          
//            DATACLAS=PSEN,MGMTCLAS=NOBACKUP                                   
//SYSIN    DD DUMMY                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J210                                         
