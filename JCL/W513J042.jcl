//W513J042 JOB (650W5130100W513J042,W100),'RTN W513V1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W513    EXEC W513P042                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W513J042                                         
