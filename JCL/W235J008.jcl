//W235J008 JOB (670W2350100W235J008,W100),'RTN W235B2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W235    EXEC W235P008                                                         
//*                                                                             
//W23508.W23508D1 DD *                                                          
&IDANSK-FOM.&IDANSK-TOM.&IDLEVNR.&AVR-VV.&FLSLAP.&FLLEVBE.&MAIL.                
//*                                                                             
//SOPPASS EXEC WSOP                                                             
 SET VALUE W235B2                                                               
  LEVBESK(&FLLEVBE)                                                             
 END-SET                                                                        
 IF-SYMBOL W235B2 LEVBESK(N)                                                    
    PASSIVATE W235J010                                                          
 END-IF                                                                         
//SOPEND  EXEC WSOPEND,PROCESS=W235J008                                         
