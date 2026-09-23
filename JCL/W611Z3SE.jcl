//W611Z3SE JOB (640W6110100W611Z3SE,W100),'RTN W611S3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//VCOM    EXEC W016P022,VCOM=W611Z3SE                                           
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W611S3.W61113(+0),DISP=SHR                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611Z3SE                                         
