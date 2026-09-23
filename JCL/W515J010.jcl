//W515J010 JOB (640W5100100W515J010,W100),'RTN W515D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W51510 EXEC W515P010                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W515J010                                         
