//W570J087 JOB (640W5700100W570J087,W100),'RTN W570M3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W570    EXEC W570P087                                                         
//W57087.EZTVFM DD SPACE=(8192,(10000,10000))                                   
//W57087.SORTWK01 DD  DATACLAS=PSEB                                             
//W57087.SORTWK02 DD  DATACLAS=PSEB                                             
//W57087.SORTWK03 DD  DATACLAS=PSEB                                             
//W57087.SORTWK04 DD  DATACLAS=PSEB                                             
//W57087.SORTWK05 DD  DATACLAS=PSEB                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W570J087                                         
