//W910ZBSE JOB (640W9100100W910ZBSE,W100),'RTN W910D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=WUT.W910D2.W91021(+0)                               
//VCOM     EXEC W016P022,VCOM=W910ZBSE,COND=(0,LT,TOM.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W910D2.W91021(+0),DISP=SHR                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910ZBSE                                         
