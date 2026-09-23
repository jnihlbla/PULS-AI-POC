//W412J011 JOB (640W4120100W412J011,W100),'RTN W412V3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*                                                                              
//W412    EXEC W412P011                                                         
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W412J011                                         
