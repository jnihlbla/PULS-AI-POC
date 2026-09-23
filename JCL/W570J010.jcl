//W570J010 JOB (640W5700100W570J010,W100),'RTN W570D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W57010 EXEC W570P010                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W570J010                                         
