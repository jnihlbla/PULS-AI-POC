//W810JAC2 JOB (640W8100100W810JAC2,W100),'RTN W810D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  I RFSUMIT-STEGEN NEDAN ANGES VILKA IC-JOBB                                 
//*  SOM SKA SUBMITTAS EFTER ORDINARIE KÖRNINGAR VID:                           
//*                                                                             
//*  2 : A    D A G E N    I    R E D O V I S N I N G S P E R I O D             
//*                                                                             
//*SUB1   EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=XXXXXXXX                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W810JAC2                                         
/*                                                                              
