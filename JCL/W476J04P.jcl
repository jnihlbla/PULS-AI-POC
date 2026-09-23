//W476J04P JOB (540W4760100W476J04P,W100),'RTN W476SP',                         
//             CLASS=L,                                                         
//             USER=?,PASSWORD=?                                                
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W476    EXEC W476P04P                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W476J04P                                         
