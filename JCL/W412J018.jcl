//W412J018 JOB (670W4120100W412J018,W100),'RTN W412S4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W412    EXEC W412P018                                                         
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER W412S5                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412J018                                         
