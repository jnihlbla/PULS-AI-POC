//W970J043 JOB (640W0000100W970J043,W100),'RTN W970V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST9                                                    
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W970    EXEC W970P043,YYWW=&YYWW                                              
//*                                                                             
//DAP     EXEC WZ14DAP2,DSIN=W970.W970V2.W97043.YYWW&YYWW                       
//SYSIN        DD *                                                             
W970V2                                                                          
SOP                                                                             
&YYWW                                                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J043                                         
