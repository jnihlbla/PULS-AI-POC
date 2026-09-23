//W571J002 JOB (670W5710100W571J002,W100),'RTN W571B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W571    EXEC W571P002                                                         
//*                                                                             
&IDDC                                                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W571J002                                         
