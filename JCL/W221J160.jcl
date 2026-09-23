//W221J160 JOB (670W2210200W221J160,W100),'RTN W221D5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*NTL XXXXXXXX,EXC                                                             
//W221    EXEC W221P160                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J160                                         
