//W271JA12 JOB (640W2710100W271JA12,W100),'RTN W271B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//*+JBS BIND IMG0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P012,REFILL=B1,                                              
//             INDUT2=W271.W271B1                                               
//*                                                                             
//W27112.SYSIN    DD *                                                          
&ORDERTYP,&DC,&DIST,&KUND,&LEVNR                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271JA12                                         
