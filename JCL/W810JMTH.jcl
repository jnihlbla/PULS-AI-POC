//W810JMTH JOB (640W8100100W810JMTH,W100),'RTN W810D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ LOCAL                                                              
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  I WFSUBMIT-STEGEN NEDAN ANGES DE IC-JOBB                                   
//*  SOM SKA SUBMITTAS EFTER ORDINARIE KÖRNINGAR VID:                           
//*                                                                             
//*     M O N T H L Y  (END OF MONTH)                                           
//*                                                                             
//SUB30 EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=AAAMONTH                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W810JMTH                                         
/*                                                                              
