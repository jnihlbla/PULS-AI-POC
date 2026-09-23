//W810JPM2 JOB (640W8100100W810JPM2,W100),'RTN W810D1',                         
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
//*        P R O G R A M P E R I O D S L U T - 2                                
//*                                                                             
//*SUB1   EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=XXXXXXXX                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W810JPM2                                         
/*                                                                              
