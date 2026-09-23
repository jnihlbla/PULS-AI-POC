//W810JDAY JOB (640W8100100W810JDAY,W100),'RTN W810D1',                         
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
//*                                                                             
//*SUB1    EXEC WFSUBMIT,JCLLIB=W.QASE.JCL,SUBMIT=W289J100                      
//*                                                                             
//SUB2    EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=RORAD                           
//* NEDANSTÅENDE UPPLAGD PÅ BEGÄRAN AV HENRIK DAHLBOM    121127                 
//SUB3    EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=AAADAY                          
//*                                                                             
//SUB4    EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=WDK9FIL                         
//* NEDANSTÅENDE UPPLAGD PÅ BEGÄRAN AV BO RUNDBERG   SG  960902                 
//SUB5    EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=FK17ES                          
//* NEDANSTÅENDE UPPLAGD PÅ BEGÄRAN AV BO RUNDBERG   BS  980210                 
//SUB6    EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=DEVDAYF                         
//* NEDANSTÅENDE UPPLAGD PÅ BEGÄRAN AV BJÖRN JENSEN   KJ  110421                
//SUB8    EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=LDCRADAG                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W810JDAY                                         
