//W221J060 JOB (670W2210200W221J060,W100),'RTN W221D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*NTL XXXXXXXX,EXC                                                             
//W221    EXEC W221P060                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J060                                         
