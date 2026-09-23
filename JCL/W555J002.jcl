//W555J002 JOB (650W5550100W555J002,W100),'RTN W555V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W555    EXEC W555P002                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W555J002                                         
