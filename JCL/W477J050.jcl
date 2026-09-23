//W477J050 JOB (540W4750100W477J050,W100),'RTN W477M1',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W477    EXEC W477P050                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W477J050                                         
