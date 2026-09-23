//W810JWEE JOB (640W8100100W810JWEE,W100),'RTN W810D1',                         
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
//*                  V E C K O S L U T                                          
//*                                                                             
//* NEDANSTÅENDE PÅ BEGÄRAN AV HENRIK DAHLBOM            KA 140425              
//* DET ERSÄTTER ALLA TIDIGARE SUBMITTER SOM LÅG I                              
//* DETTA JOBB OCH SOM NU FLYTTATS TILL NEDANSTÅENDE                            
//*                                                                             
//SUB30  EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=AAAWEEK                          
//*                                                                             
//SOP2    EXEC WSOP                                                             
IF-CALENDAR ODDWEEK                                                             
  ORDER W810JODD                                                                
ENDIF                                                                           
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W810JWEE                                         
