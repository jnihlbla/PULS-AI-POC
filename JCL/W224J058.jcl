//W224J058 JOB (640W2240100W224J058,W100),'RTN W224S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* LEV=&IDLEVNR,DC=&DC                                                         
//*                                                                             
//W224    EXEC W224P058                                                         
//*                                                                             
//* PARAMETER FILE FROM 2111                                                    
//W22458.W22458D1 DD *                                                          
&IDLEVNR,&DC                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224J058                                         
