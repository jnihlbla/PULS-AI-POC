//W551J067 JOB (670W5510100W551J067,W100),'RTN W551B6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W551    EXEC W551P067,TYP=&MCOMP                                              
//*                                                                             
//*  ---------- "SOP SYMBOL VALUES" för denna körning ------------              
//*                                                                             
//* MCOMP(&MCOMP)                                                               
//*                                                                             
//VCOM     EXEC W016P022,VCOM=W551Z1&MCOMP                                      
//W01622.W016ZZD1 DD DSN=W551.W551B6.W55167(+1),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551J067                                         
