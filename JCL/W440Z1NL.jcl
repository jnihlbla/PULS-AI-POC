//W440Z1NL JOB (640W4600100W440Z1NL,W100),'RTN W440V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* BO-REPORT  (LISTA) HOLLAND                                                  
//*                                                                             
//VCOM     EXEC W016P022,VCOM=W440Z1NL                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W440.W440V1.W4405I(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W440Z1NL                                         
