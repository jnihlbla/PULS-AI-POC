//W910ZJDK JOB (640W9100100W910ZJDK,W100),'RTN W910V4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* TOTALFIL MOTSVARANDE W910ZEDK UPPDATERINGSFIL                               
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=WUT.W910V4.W91065(+0)                               
//VCOM     EXEC W016P022,VCOM=W910ZEDK,COND=(0,LT,TOM.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W910V4.W91065(+0),DISP=SHR                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910ZJDK                                         
