//W970J035 JOB (670W0000100W970J035,W100),'RTN W970B3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ   NJEVC                                                            
/*ROUTE  PRINT NJOVC                                                            
//*+JBS BIND IMG0                                                               
//*                                                                             
// EXEC WZ14PDAP,DSIN=W970.W970B3.W97030(+0)                                    
//SYSIN        DD *                                                             
W970-LSTRULE                                                                    
&ACLGRP                                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J035                                         
