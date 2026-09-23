//W810JTHU JOB (640W8100100W810JTHU,W100),'RTN W810D1',                         
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
//*     T O R S D A G A R                                                       
//*                                                                             
//*SUB1   EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=XXXXXXX                         
//*                                                                             
//* NEDANSTÅENDE INLAGDA PÅ UPPDRAG AV T.RYBERG  PF 950626                      
//SUB34  EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=EDIKOLL                          
//* NEDANSTÅENDE INLAGDA PÅ UPPDRAG AV PATRIK LINDGREN 051221                   
//SUB36  EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=URSKOLL2                         
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W810JTHU                                         
/*                                                                              
