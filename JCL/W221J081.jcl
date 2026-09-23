//W221J081 JOB (670W2210200W221J081,W100),'RTN W221D5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*NTL XXXXXXXX,EXC                                                             
//W221    EXEC W221P081                                                         
//*                                                                             
//W22181T EXEC WEMPTST,DSIN=W221.W221D5.W22181(+1)                              
//PASSIV  EXEC WSOP,COND=(0,EQ,W22181T.T)                                       
PASSIVATE W221J160                                                              
PASSIVATE W221J162                                                              
PASSIVATE W221ZASE                                                              
PASSIVATE W221J133                                                              
PASSIVATE W221Z4SE                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J081                                         
