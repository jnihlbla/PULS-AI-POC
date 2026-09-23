//W030J014 JOB (640W0010300W030J014,W100),'RTN W030D1',                         
//             USER=?,PASSWORD=?,                                               
//            CLASS=K                                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W030    EXEC W030P014                                                         
//DATUM   EXEC WDATUM                                                           
//SYSIN     DD DSN=W030.W030D1.W03014(+1),DISP=(OLD,KEEP,KEEP)                  
//*                                                                             
//VRCABE  EXEC VRCABEND,COND=(8,GT)                                             
//SOP     EXEC WSOPEND,PROCESS=W030J014                                         
