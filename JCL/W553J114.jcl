//W553J114 JOB (670W5530100W553J014,W100),'RTN W553B2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W553    EXEC W553P014,                                                        
//        INDIN=&W553..W553B2                                                   
//*                                                                             
//W55314.W55314D1 DD  DSN=&INDIN..W55313(+0),DISP=SHR                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W553J114                                         
