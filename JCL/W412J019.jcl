//W412J019 JOB (670W4120100W412J019,W100),'RTN W412D8',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W412    EXEC W412P019                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412J019                                         
