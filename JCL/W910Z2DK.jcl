//W910Z2DK JOB (640W9100100W910Z2DK,W100),'RTN W910B7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//VCOM     EXEC W016P022,VCOM=W910Z1DK                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W910.W910B6.W91068(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910Z2DK                                         
