//W970J002 JOB (640W0000100W970J002,W100),'RTN W970V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLKFKT EXEC WEZTP,                                                            
//             LIB=W.QASE.EPLUS,                                                
//             MEMBER=W9700200                                                  
//INFIL     DD DSN=W970.W970B1.W97001(+0),DISP=SHR                              
//UTFIL1    DD DSN=W970.W970B1.W97002(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160),                            
//             SPACE=(TRK,(10,10),RLSE),MGMTCLAS=NOBACKUP                       
//UTFIL2    DD DSN=W970.W970B1.W97002(+2),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160),                            
//             SPACE=(TRK,(10,10),RLSE),MGMTCLAS=NOBACKUP                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J002                                         
