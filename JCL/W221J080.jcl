//W221J080 JOB (670W2210200W221J080,W100),'RTN W221D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*NTL XXXXXXXX,EXC                                                             
//W221    EXEC W221P080                                                         
//*                                                                             
//W22181T EXEC WEMPTST,DSIN=W221.W221D2.W22181(+1)                              
//PASSIV  EXEC WSOP,COND=(0,EQ,W22181T.T)                                       
PASSIVATE W221J060                                                              
PASSIVATE W221J062                                                              
PASSIVATE W221Z1SE                                                              
PASSIVATE W221J033                                                              
PASSIVATE W221Z3SE                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J080                                         
