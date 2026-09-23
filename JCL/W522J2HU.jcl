//W522J2HU JOB (670W5220100W522J2HU,W100),'RTN W522M1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W522    EXEC W522P021                                                         
//*                                                                             
HUXXXXXXXXX                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W522J2HU                                         
