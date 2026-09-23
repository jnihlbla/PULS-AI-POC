//W810JACC JOB (640W8100100W810JACC,W100),'RTN W810D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  I WFSUBMIT-STEGEN NEDAN ANGES VILKA IC-JOBB                                
//*  SOM SKA SUBMITTAS EFTER ORDINARIE KÖRNINGAR VID:                           
//*                                                                             
//*       R E D O V I S N I N G S P E R I O D S L U T                           
//*                                                                             
//*                                                                             
//SUB1   EXEC WFSUBMIT,JCLLIB=W.QASE.JCL,SUBMIT=W810J001                        
//* DELAY THE SUBMIT TO AVOID ABEND U0457 IN D&P STEP                           
//WAIT    EXEC WWAIT,SECONDS=15                                                 
//SUB2   EXEC WFSUBMIT,JCLLIB=W.QASE.JCL,SUBMIT=W810J002                        
//* DELAY THE SUBMIT TO AVOID ABEND U0457 IN D&P STEP                           
//WAIT    EXEC WWAIT,SECONDS=15                                                 
//SUB3   EXEC WFSUBMIT,JCLLIB=W.QASE.JCL,SUBMIT=W810J003                        
//* NEDANSTÅENDE PÅ BEGÄRAN AV HENRIK DAHLBOM/ANNA D.    MB 011126              
//SUB4   EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=BUFFE2AD                         
//* NEDANSTÅENDE PÅ BEGÄRAN AV ÖSTEN ARVIDSSON           MB 020626              
//SUB5   EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=EXP8                             
//* NEDANSTÅENDE PÅ BEGÄRAN AV PATRIK LINDGREN           JN 051221              
//SUB6   EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=RENUPPF                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W810JACC                                         
