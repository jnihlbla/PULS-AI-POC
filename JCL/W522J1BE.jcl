//W522J1BE JOB (650W5220100W522J1BE,W100),'RTN W522M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=999                                         
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W522    EXEC W522P114,                                                        
//             SOUT1='(A,,INTB)'                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W522J1BE                                         
