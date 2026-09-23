//W412J002 JOB (670W4120100W412J002,W100),'RTN W412S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W412    EXEC W412P002                                                         
//W41202D1  DD                                                                  
//          DD DSN=&XTRAFILE,DISP=SHR                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412J002                                         
