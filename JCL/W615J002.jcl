//W615J002 JOB (670W6150100W615J002,W100),'RTN W615S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W615    EXEC W615P002                                                         
//*                                                                             
//W61502.SYSIN DD *                                                             
&A,&B,&C,&D,&E,&F,&G,&H,&I,&J,&K,&L,                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W615J002                                         
