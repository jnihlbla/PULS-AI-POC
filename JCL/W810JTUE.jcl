//W810JTUE JOB (640W8100100W810JTUE,W100),'RTN W810D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ LOCAL                                                              
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  I WFSUBMIT-STEGEN NEDAN ANGES DE IC-JOBB                                   
//*  SOM SKA SUBMITTAS EFTER ORDINARIE KÖRNINGAR PÅ:                            
//*                                                                             
//*     T I S D A G A R   (ONSDAG MORGON)                                       
//*                                                                             
//*SUBA   EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=XXXXXXX                         
//*                                                                             
//* NEDANSTÅENDE INLAGDA PÅ UPPDRAG AV T.RYBERG  PF 950626                      
//SUB34  EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=EDIKOLL                          
//* NEDANSTÅENDE INLAGDA PÅ UPPDRAG AV ANNA MFL  MB 012221                      
//SUB40  EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=BUFFERML                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W810JTUE                                         
/*                                                                              
