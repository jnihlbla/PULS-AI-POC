//W810JWK1 JOB (640W8100100W810JWK1,W100),'RTN W810D1',                         
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
//*     1 : A   K Ö R N I N G S D A G E N   I   V E C K A N                     
//*                                                                             
//SUB2   EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=TIDISPIN                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W810JWK1                                         
/*                                                                              
