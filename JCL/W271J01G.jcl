//W271J01G JOB (640W2710100W271J01G,W100),'RTN W271S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* SOP-PARAMETERS FOR THIS JOB:                                                
//*     DC    : &DC                                                             
//*     OPTION: &OPTION                                                         
//*     EMAIL : &EMAIL                                                          
//*                                                                             
//W271    EXEC W271P01G                                                         
//W2711G.SYSIN    DD *                                                          
&DC,&OPTION                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J01G                                         
