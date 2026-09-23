//W910Z9SE JOB (640W9100100W910Z9SE,W100),'RTN W910V4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* SKICKAR FÖREGÅENDE VECKAS FIL                                               
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=WUT.W910V4.W91076(-1)                               
//VCOM     EXEC W016P022,VCOM=W910Z9SE,COND=(0,LT,TOM.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W910V4.W91076(-1),DISP=SHR                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910Z9SE                                         
