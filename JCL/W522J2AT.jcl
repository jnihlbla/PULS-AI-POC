//W522J2AT JOB (670W5220100W522J2AT,W100),'RTN W522M1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W522    EXEC W522P021                                                         
//*                                                                             
ATXXXXXXXXX                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W522J2AT                                         
