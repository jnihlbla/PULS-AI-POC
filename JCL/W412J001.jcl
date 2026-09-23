//W412J001 JOB (640W4120100W412J001,W100),'RTN W412S1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W412    EXEC W412P001,INDIN=W412.&ORDTYP                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412J001                                         
