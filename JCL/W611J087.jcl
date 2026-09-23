//W611J087 JOB (640W6110100W611J087,W100),'RTN W611V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W611    EXEC W611P087                                                         
//*                                                                             
//MAIL  EXEC WMAILSND                                                           
)SEND                                                                           
TITLE REFILL CD                                                                 
TO MAIIM@VOLVOCARS.COM                                                          
TO LNILSS50@VOLVOCARS.COM                                                       
ATTACH W611.W611V3.W61187(+1)  W61187.CSV TEXT                                  
MAIL                                                                            
 REFILLORDERFÖRDELNING,                                                         
 CROSSDOCKING ARTIKLAR"                                                         
)END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J087                                         
