//W116J0Z5 JOB (640W1160100W116J0Z5,W100),'RTN W116SB',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//VCOM     EXEC W016P022,VCOM=&VCOM2                                            
//*                                                                             
//W01622.W016ZZD1 DD DSN=W116.W116SB.W11687(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J0Z5                                         
