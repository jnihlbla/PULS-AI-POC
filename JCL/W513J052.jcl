//W513J052 JOB (670W5100100W513J052,W100),'RTN W513S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W513    EXEC W513P052                                                         
//W51352.W51352D1 DD *                                                          
&URVAL1.                                                                        
//*                                                                             
//MAIL  EXEC WMAILSND                                                           
)SEND                                                                           
TITLE PARTINFO                                                                  
TO &MAIL                                                                        
ATTACH W513.W513S1.W51352(+1)  W51352.TXT TEXT                                  
MAIL                                                                            
 PARTINFO,                                                                      
 ORDERED FROM SCREEN 5311/5312"                                                 
)END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W513J052                                         
