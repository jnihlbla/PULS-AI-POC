//W335J15A JOB (670W3350100W335J15A,W100),'RTN W335B3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* ERSÄTTNINGAR "REFRESHING" BESTÄLLES VIA W335B3M0 VCAS                       
//*                                         W335B3M1 SVERIGE                    
//*                                         W335B3M2 VCEM                       
//*                                         W335B3M3 ASIA PACFIC                
//*                                         W335B3M4 VCSA SOUTH AMERICA         
//*                                         W335B3M5 VCNA                       
//*                                         W335B3M6 VCI                        
//*         VCOM PARAMETERN W335Z3M(+MBNUMMER)                                  
//*                                                                             
//VCOM     EXEC W016P022,VCOM=&VCOM                                             
&VCOM                                                                           
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335B3.W33553(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J15A                                         
