//W235J011 JOB (640W2350100W235J011,W100),'RTN W235B3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//* ANSK-FOM = &IDANSK-FOM                                                      
//* ANSK-TOM = &IDANSK-TOM                                                      
//* LEVNR    = &IDLEVNR                                                         
//* IDDC     = &IDDC                                                            
//* AVR-VV   = &AVR-VV                                                          
//* FLSLAP   = &FLSLAP                                                          
//* FLLEVB   = &FLLEVBE                                                         
//* MAIL     = &MAIL                                                            
//*                                                                             
//W235    EXEC W235P011                                                         
//*                                                                             
//W23511.W23511D1 DD *                                                          
&IDANSK-FOM.&IDANSK-TOM.&IDLEVNR.&IDDC.&AVR-VV.&FLSLAP.&FLLEVBE.&MAIL.          
/*                                                                              
//SOPPASS EXEC WSOP                                                             
 SET VALUE W235B3                                                               
  LEVBESK(&FLLEVBE)                                                             
 END-SET                                                                        
 IF-SYMBOL W235B3 LEVBESK(N)                                                    
    PASSIVATE W235J012                                                          
 END-IF                                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W235J011                                         
