//W810JTIM JOB (640W8100100W810JTIM,W100),'RTN W810S1',                         
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
//*            V A R J E   D A G                                                
//*           (MÅNDAG - FREDAG )                                                
//*            K L  1 1 . 0 0                                                   
//*            K L  1 5 . 0 0                                                   
//*           EFTER NYTT EXTRAKTLAGERBAND                                       
//*                                                                             
//SUB1    EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=LOSPF20                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W810JTIM                                         
/*                                                                              
