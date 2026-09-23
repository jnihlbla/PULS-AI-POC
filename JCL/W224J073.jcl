//W224J073 JOB (640W2240100W224J073,W100),'RTN W224S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
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
//W224    EXEC W224P073                                                         
//*                                                                             
//* PARAMETER FILE FROM SCREEN 2111 AND 2115                                    
//W22473.W22473D1 DD *                                                          
&IDLEVNR,&DC,&AT,&LT,&TT                                                        
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W224J073                                         
