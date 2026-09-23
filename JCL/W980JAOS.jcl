//W980JAOS JOB (650W0090100W980JAOS,W100),'RTN W980V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800                                                            
/*ROUTE XEQ    LOCAL                                                            
/*ROUTE PRINT  LOCAL                                                            
//*                                                                             
//AOSRPT  EXEC VLOGON,INDEX=F1AOVC,GROUP=AOS                                    
//SYSPROC  DD  DUMMY                                                            
//PUTDATA  DD  DSN=W980.W980V1.W98032(+0),DISP=(MOD,KEEP)                       
//SYSTSIN  DD  *                                                                
ISPSTART CMD(V140IB1 LASTWEEK W* VCC)                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W980JAOS                                         
