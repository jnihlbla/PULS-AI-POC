//W461J12O JOB (650W4610100W461J12O,W100),'RTN W461V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W461    EXEC W461P02O,                                                        
//             INDIN=W461.W461V1,                                               
//             INDUT=W461.W461V1                                                
//SOP     EXEC WSOPEND,PROCESS=W461J12O                                         
