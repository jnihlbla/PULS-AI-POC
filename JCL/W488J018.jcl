//W488J018 JOB (540W4880100W488J018,W100),'RTN W488S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//VCOM    EXEC W016P022,VCOM=W488Z1SE                                           
//*                                                                             
//W01622.W016ZZD1 DD DSN=W488.W488S1.DELSALDO,DISP=SHR                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W488J018                                         
