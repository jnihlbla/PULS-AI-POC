//W615J004 JOB (670W6150100W615J004,W100),'RTN W615S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W615    EXEC W615P004                                                         
//*                                                                             
//W61504.SYSIN DD *                                                             
&A,&B,&C,&D,&E,&F,&G,&H,&I,&J,&K,&L,                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W615J004                                         
