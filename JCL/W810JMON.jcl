//W810JMON JOB (640W8100100W810JMON,W100),'RTN W810D1',                         
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
//*     M Å N D A G A R  (TISDAG MORGON)                                        
//*                                                                             
//SUB30 EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=AAATUESD                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W810JMON                                         
/*                                                                              
