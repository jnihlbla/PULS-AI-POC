//W224J074 JOB (640W2240100W224J074,W100),'RTN W224S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*  IDLEVNR = &IDLEVNR                                                         
//*       DC = &DC                                                              
//*       AT = &AT                                                              
//*       LT = &LT                                                              
//*       TT = &TT                                                              
//*                                                                             
//W224    EXEC W224P074                                                         
//*                                                                             
//* PARAMETER FILE FROM SCREEN 2111 AND 2115                                    
//W22474.W22474D1 DD *                                                          
&IDLEVNR,&DC,&AT,&LT,&TT                                                        
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W224J074                                         
