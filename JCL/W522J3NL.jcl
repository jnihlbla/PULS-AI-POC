//W522J3NL JOB (650W5220100W522J3NL,W100),'RTN W522M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=999                                         
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W522    EXEC W522P122,                                                        
//             SOUT1='(A,,VATD)'                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W522J3NL                                         
