//W611Z5SE JOB (640W6110100W611Z5SE,W100),'RTN W611S5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//VCOM    EXEC W016P022,VCOM=W611Z5SE                                           
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W611S5.W61161(+0),DISP=SHR                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611Z5SE                                         
