//W460J0Z1 JOB (640W4600100W460J0Z1,W100),'RTN W460S3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* NOAC LISTA (INFO) DAGLIGEN                                                  
//*                                                                             
//VCOM     EXEC W016P022,VCOM=&VCOM2                                            
//*                                                                             
//W01622.W016ZZD1 DD DSN=W460.W460S3.W46055(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W460J0Z1                                         
