//W555J014 JOB (650W5550100W555J014,W100),'RTN W500M1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W555    EXEC W555P014                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W555J014                                         
