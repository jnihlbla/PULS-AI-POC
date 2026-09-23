//W030J015 JOB (650W0010300W030J015,W100),'RTN W030D2',                         
//             USER=?,PASSWORD=?,                                               
//            CLASS=K                                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W030    EXEC W030P015                                                         
//DATUM   EXEC WDATUM                                                           
//SYSIN     DD DSN=W030.W030D2.W03015(+1),DISP=(OLD,KEEP,KEEP)                  
//VRCABE  EXEC VRCABEND,COND=(8,GT)                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W030J015                                         
