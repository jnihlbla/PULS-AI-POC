//W116J0Z2 JOB (640W1160100W116J0Z2,W100),'RTN W116S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//VCOM     EXEC W016P022,VCOM=&VCOM2                                            
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W116S2.W11626(+0),DISP=SHR                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J0Z2                                         
