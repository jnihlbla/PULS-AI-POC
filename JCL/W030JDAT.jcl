//W030JDAT JOB (640W0010300W030JDAT,W100),'RTN WYR001',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*--------------------------------------------------*                          
//*  NEDANSTÅENDE PARAMETER ÄR RÄTT                  *                          
//*---------------------------VV---------------------*                          
//W030    EXEC W030P012,DATUM=&YY.1231                                          
//*                                                                             
//DATUM   EXEC WDATUM                                                           
//SYSIN     DD DSN=W030.W030D2.W03012(+1),DISP=(OLD,KEEP,KEEP)                  
//*                                                                             
//W030W   EXEC W030P016                                                         
//SYSUT1    DD DSN=W.QASE.DATUM(+1)                                             
//*                                                                             
//VRCABE  EXEC VRCABEND,COND=(8,GT)                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W030JDAT                                         
