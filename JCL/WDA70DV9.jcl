//WDA70DV9 JOB (650W0020200WDA70DV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDA7    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDA7K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(18000,1800),RLSE),                                  
//             DCB=BLKSIZE=27998,                                               
//             MGMTCLAS=EMID70                                                  
//DMP.WDA7K  DD DSN=WG01.QASE.WDA7K,DISP=SHR,                                   
//             AMP=('BUFND=13,BUFNI=10')                                        
//DMP.SYSIN DD  *                                                               
D1 WDA7     WDA7K    OUTDD1                                                     
//*                                                                             
//DLET    EXEC PGM=IEFBR14,COND=(0,LT,WDA7.DMP)                                 
//DD1     DD DSN=WG01.UNLO.WDA7K(0),DISP=(OLD,DELETE)                           
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDA70DV9                                         
