//W510D7RS JOB (640W5100100W510D7RS,W100),'RTN W510D7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W510D7                                               
//*                                                                             
//RENAME  EXEC WRTNINP,                                                         
//             F1=W213.W200D1.W21350,                                           
//             T1=W213.W510D7.W21350,RF1=FB,LR1=12,                             
//*                                                                             
//             F2=W213.W213D2.W21352A,                                          
//             T2=W213.W510D7.W21352A,RF2=FB,LR2=12,                            
//*                                                                             
//             F3=W213.W213D3.W21352B,                                          
//             T3=W213.W510D7.W21352B,RF3=FB,LR3=12                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510D7RS                                         
