//W236J011 JOB (640W2360100W236J011,W100),'RTN W236D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W015P08X EXEC W015P08X,                                                       
//             DSOUT='W236.W236D5.WDD9UNLD(+1)',                                
//             PSB=WDD9BL                                                       
//SYSIN    DD *                                                                 
****                                                                            
00                                                                              
**                                                                              
**                                                                              
//WDD9V      DD  DSN=WG01.QASE.WDD9V,DISP=SHR                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W236J011                                         
