//W553J122 JOB (640W5530100W553J122,W100),'RTN W553B3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W553    EXEC W553P022,                                                        
//             INDIN=&W553..W553B3                                              
//W55322.W55322D1 DD  DSN=&INDIN..W55305(+0),DISP=SHR                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W553J122                                         
