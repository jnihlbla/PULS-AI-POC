//W614J022 JOB (640W6140100W614J022,W100),'RTN W614S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W614    EXEC W614P022                                                         
//W61422.W61422D1 DD *                                                          
&URVAL                                                                          
&IDMAIL1                                                                        
&IDMAIL2                                                                        
&IDMAIL3                                                                        
&IDMAIL4                                                                        
//*                                                                             
//MEMO    EXEC WMEMOSND,DSIN=W614.W614S2.W61421(0)                              
//APIFILE DD   DSN=W614.W614S2.W61422(+1),DISP=SHR                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W614J022                                         
