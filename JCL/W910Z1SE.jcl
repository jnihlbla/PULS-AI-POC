//W910Z1SE JOB (670W9100100W910Z1SE,W100),'RTN W910P1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*      ÖVERFÖRING REGRESS   LEVERANTÖRDATA                                    
//*************  VCAS                                                           
//VCOM     EXEC W016P022,VCOM=W910Z1SE                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W910.W910P1.W91083(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910Z1SE                                         
