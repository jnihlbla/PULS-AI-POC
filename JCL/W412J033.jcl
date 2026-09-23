//W412J033 JOB (670W4120100W412J033,W100),'RTN W412D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*AFTER FIMG0AXX                                                                
//W412    EXEC W412P033                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412J033                                         
