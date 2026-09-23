//W810JODD JOB (640W8100100W810JODD,W100),'RTN W810D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ LOCAL                                                              
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  I WFSUBMIT-STEGEN NEDAN ANGES VILKA IC-JOBB                                
//*  SOM SKA SUBMITTAS EFTER ORDINARIE KÖRNINGAR VID:                           
//*                                                                             
//*                  V E C K O S L U T, U D D A V E C K O R                     
//*                                                                             
//* NEDANSTÅENDE PÅ BEGÄRAN AV WOLFGANG KUX (BEREDN)                            
//SUB01  EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=WKUX01  /CE 2006-04-24           
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W810JODD                                         
