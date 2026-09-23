//W570J068 JOB (640W5700100W570J068,W100),'RTN W570D3',                         
//             USER=?,PASSWORD=?,CLASS=K                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W570    EXEC W570P068                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W570J068                                         
