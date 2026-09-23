//W271J029 JOB (640W2710100W271J029,W100),'RTN W271B2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=999                                         
//*+JBS BIND VCC1                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P029                                                         
//*                                                                             
//W27129.SYSIN    DD *                                                          
&BEST                                                                           
//*                                                                             
//MAIL  EXEC WMAILSND                                                           
)SEND                                                                           
TITLE PARTINFO                                                                  
TO    &MAIL                                                                     
ATTACH  W271.W271B2.W27129(+1) W27129.XLS TEXT                                  
MAIL                                                                            
 PARTINFO,                                                                      
 ORDERED FROM SCREEN 2348                                                       
)END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J029                                         
