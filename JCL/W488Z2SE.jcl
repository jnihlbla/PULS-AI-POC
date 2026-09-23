//W488Z2SE JOB (540W4880100W488Z2SE,W100),'RTN W488D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//VCOM    EXEC W016P022,VCOM=W488Z2SE                                           
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W488D1.W48858(+0),DISP=SHR                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W488Z2SE                                         
