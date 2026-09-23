//W970J039 JOB (670W0000100W970J035,W100),'RTN W970B3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE  PRINT LOCAL                                                            
//*+JBS BIND IMG0                                                               
//*                                                                             
//ACF     EXEC FLOGON                                                           
//SYSTSPRT DD  DSN=&&W97039,DISP=(NEW,PASS,DELETE),                             
//             RECFM=VBA,LRECL=133,                                             
//             DATACLAS=PSEN                                                    
//SYSTSIN  DD  DSN=W.QASE.CONSTANT(W970PWDC),DISP=SHR                           
//*                                                                             
// EXEC WZ14PDAP,DSIN=&&W97039                                                  
//SYSIN        DD *                                                             
W970-LSTRULE                                                                    
PWDCHANGE                                                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J039                                         
