//W224J021 JOB (640W2240100W224J021,W100),'RTN W224B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* IDLEVNR = &IDLEVNR                                                          
//* IDDC    = &DC                                                               
//*                                                                             
//W224    EXEC W224P021                                                         
//*                                                                             
//W22421.SYSIN    DD *                                                          
&IDLEVNR,&DC                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224J021                                         
