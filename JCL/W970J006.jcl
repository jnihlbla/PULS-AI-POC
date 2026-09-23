//W970J006 JOB (640W0000100W970J006,W100),'RTN W970V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//VALJHIXW EXEC WEZTP,                                                          
//             LIB=W.QASE.EPLUS,                                                
//             MEMBER=W9700600                                                  
//EZTVFM    DD SPACE=(CYL,(40,10))                                              
//INFILA    DD DSN=W970.W970B1.W97002(+0),DISP=SHR                              
//INFILH    DD DSN=W970.W970B1.W97005(+0),DISP=SHR                              
//UTFIL     DD DSN=W970.W970B1.W97006(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=38,BLKSIZE=27968),                           
//             SPACE=(TRK,(10,10),RLSE),UNIT=PLP                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J006                                         
