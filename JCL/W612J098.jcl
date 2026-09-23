//W612J098 JOB (670W6120100W612J098,W100),'RTN W612D8',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W612    EXEC W612P098                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J098                                         
